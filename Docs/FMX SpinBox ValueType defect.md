Explains TLabeledSpinBox.SetValueType in c:\Projects\LightSaber\FrameFMX\LightFmx.Visual.SpinBox.pas

# The FMX defect: changing ValueType changes Value

Changing `ValueType` must not change `Value` - but FMX changes it. Switching Float -> Integer multiplies `Value` by 10^DecimalDigits, then clamps the result into `Min..Max`.

## What FMX actually does

Traced through the Delphi 13 RTL. The text in the box is re-filtered when the filter characters change, and the value is then read back FROM that text:

```
TEditBoxModel.SetValueType      FMX.EditBox.pas    FilterChar := '0123456789-+'   (no '.' , no ',')
TCustomEditModel.SetFilterChar  FMX.Edit.pas       re-filters the EXISTING TEXT
                                                   -> '20.00' becomes '2000'
                                                   text changed -> Change
TCustomEditModel.Change         FMX.Edit.pas       -> Validate -> DoValidate
TEditBoxModel.DoValidate        FMX.EditBox.pas    ValueRange.Value := 2000  (Value re-read FROM the text)
TCustomValueRange.IntChanged    FMX.StdActns.pas   clamps into Min..Max -> 200
```

## Measured

A box left at the constructor defaults (Float, DecimalDigits=2) holding 20 with Max=200 comes out as 200 the instant `ValueType` is set to Integer. It is not a streaming bug - a plain runtime assignment triggers it just as hard.

## Exactly what moves, and when the switch is harmless

The switch scales `Value` by 10^DecimalDigits, because the decimals of the rendered text are promoted to units. It then clamps the result into `Min..Max`. So the value ends up:

- ON `Max` for a large positive value;
- on `Min` for a large negative one;
- at `Value * 10^DecimalDigits` when the scaled number still fits.

The switch is harmless in only two cases:

- **Value = 0.** `'0.00'` filters down to `'000'`, which is still zero.
- **DecimalDigits = 0.** The text then carries no separator, so the filter finds nothing to strip and no `Change` fires at all.

Whether a caller notices depends on whether it assigns `Value` afterwards - a later assignment repairs the damage by accident. The pattern that leaves the damage visible is a design-time `Value` in the `.fmx` file with `ValueType` set in code afterwards.

## Why the repair works

`TLabeledSpinBox.SetValueType` saves `Value` across the switch and assigns it back afterwards. Assigning it afterwards is enough: by then `FilterChar` is already the integer set, so the text round-trip cannot fire a second time.

`OnChange` and `OnChangeTracking` are muted for the duration because changing the DISPLAY type is not a value change. Without muting, a consumer's `OnChange` would see the bogus 200 before the restore lands.

The one case where the value legitimately does move is Float -> Integer over a fraction. That one is rounded on purpose and reported with a single `OnChange` after the handlers are restored.
