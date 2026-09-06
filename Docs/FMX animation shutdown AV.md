Explains TExitAnim.FallDown, CollectFallTargets and TAnimDoneBridge.Fire in c:\Projects\LightSaber\FrameFMX\LightFmx.Visual.Animations.pas

# The $8080808C shutdown access violation in the FMX exit animation

One crash, reached by two different routes. Both routes are closed in the current code; this file keeps the investigation that closed them, so nobody re-opens either one.

`$80808080` is FastMM's fill pattern for a freed block. `$8080808C` is that pattern plus `$0C` - an offset into a virtual method table that no longer exists, because the object was freed. The crashing frame walks a list at struct offset `$5C` on `TFmxObject` (`FFreeNotifies`) and calls a virtual method on each entry.

## Route 1 - TExitAnim.FallDown reparented the falling controls

**Symptom.** Access violation `$C0000005` at shutdown, 'read of address 0x8080808c', inside `TFmxObject.BeforeDestruction`.

**Trigger.** `TExitAnim.FallDown` used to reparent cascade targets (buttons, layouts, TreeViewItems) OUT of their original parents and INTO `OwnerForm`. The goal was to escape the original parent's clip rect so controls could fall past the bottom edge of a scrollbox.

**Root cause.** Reparenting breaks mutual FreeNotify relationships between siblings. Most dangerously `TTreeView` against `TTreeViewItem`: TreeView items register FreeNotify with their TreeView (and the other way round through internal FMX machinery). After `FallDown`:

- TreeView becomes a direct child of `OwnerForm`.
- Each TreeViewItem also becomes a direct child of `OwnerForm`.
- `Form.FChildren` now lists them as siblings, in some order.

At program shutdown (NOT at CloseQuery):

```
DoneApplication -> TComponent.DestroyComponents -> TLightForm.Destroy ->
TCustomForm.Destroy -> TFmxObject.DoDeleteChildren
```

`DoDeleteChildren` walks the form's `FChildren` list freeing each in order. Whichever sibling dies first leaves a dangling pointer in the other's `FFreeNotifies`. When the survivor is freed, its `BeforeDestruction` iterates `FFreeNotifies` and dereferences the freed entry, so the virtual call lands on `vmtBeforeDestruction` of a dead object.

**The madExcept report for the definitive crash showed:**

```
TFmxObject.BeforeDestruction +$BC  (inside FFreeNotifies iteration)
@BeforeDestruction
TControl.Destroy      <-- Y (freed sibling inside FFreeNotifies)
TFmxObject.DoDeleteChildren
TControl.Destroy      <-- X (X frees Y as its child)
TFmxObject.DoDeleteChildren
TCustomForm.Destroy -> TLightForm.Destroy
TComponent.DestroyComponents
DoneApplication -> @Halt0 -> LearnAssist.dpr initialization
```

**Fix attempts, in order:**

1. **2026-04-21 - ForceQueue `Done()` in `TAnimDoneBridge.Fire`.**
   Addressed reentrancy of `Close` inside `TAnimation.OnFinish`.
   Result: did not eliminate the crash. Stack unchanged. Kept anyway - still correct on its own merits (it defers destruction out of the animation-manager callback). This is route 2 below.

2. **2026-04-21 - Treat `TTreeView` as a single leaf in `CollectFallTargets`** (stop diving into `TTreeViewItem`s).
   Rationale: `TTreeView` and `TTreeViewItem` hold documented mutual FreeNotify links, so reparenting items out breaks destruction.
   Result: did not eliminate the crash on its own, but still correct - other sibling FreeNotify links exist (`TMultiView`, `TSvgButton` internals, styled controls), so handling `TTreeView` alone was not enough.

3. **2026-04-21 - Remove child reparenting entirely. Animate in place.**
   Rationale: any reparenting flattens `form.FChildren` and breaks destruction order for FreeNotify-linked siblings that cannot be enumerated up front. LearnAssist's main-form layouts have `ClipChildren=False`, so an in-place fall is visually acceptable.
   Result: fix confirmed by the user on 2026-04-21. The shutdown access violation is gone. This is the current state of the code.

## Route 2 - TAnimDoneBridge.Fire called Done() synchronously

`Fire()` is called by FMX.Ani from inside `TAnimation.DoFinish`, which itself runs inside `TAnimationManager`'s animation-processing loop. So `Fire` executes on the main-thread call stack of the animation manager.

The typical caller is `TExitAnim.FallDown`. `Done()` closes the form, which triggers form destruction, which destroys all the form's children - including every `TControl` that hosted a fall-down animation, and the animations themselves (they are owned components of those controls).

If `Done()` ran synchronously inside `Fire`:

1. `Animation.OnFinish` fires, execution lands in `Fire()`, which calls `Done()`.
2. `Done()` calls `Close`, then `CloseQuery` (allowed by the `FExitAnimating` guard), then `form.Destroy`, then recursive child destruction.
3. One of the destroyed children is the control whose animation we are still inside the `OnFinish` of. That control's `OwnedComponents` include the animation itself, so `TAnimation.Destroy` runs, then `BeforeDestruction`.
4. `TFmxObject.BeforeDestruction` iterates `FFreeNotifies` (offset `$5C` in the 32-bit layout). FMX's `AnimationManager` is registered there, to be notified when observed controls die.
5. Execution eventually returns up the stack to `TAnimationManager`, which still holds a pointer to the just-freed animation in its running list. FastMM has filled that block with `$80`, so dereferencing its virtual method table reads from `$80808080`, the virtual call hits `$8080808C`, and the program access-violates inside `TFmxObject.BeforeDestruction`.

**Observed symptom.** The project raised `$C0000005` with `access violation at <anim_unit_addr>: read of address 0x8080808c` inside `TFmxObject.BeforeDestruction`, call stack `TFmxObject.BeforeDestruction -> @BeforeDestruction -> TObject.Free`.

**The fix.** Post `Done()` to the main-thread message queue with `TThread.ForceQueue`. The current `OnFinish` / manager-loop call stack unwinds fully first, and `AnimationManager` finishes processing the running list and drops its pointer to this animation. Only then does the queued closure run, close the form, and cascade destruction. No stale pointers, no access violation.

**Why the bridge is not freed inside Fire.** The bridge is owned by `OwnerForm` through normal `TComponent` ownership, so it dies with the form. Freeing it inside `Fire`, or queuing a deferred free, causes a use-after-free at finalization, because `OwnerForm`'s component-list walk would then touch a child that went early.
