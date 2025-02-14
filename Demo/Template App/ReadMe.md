This shows you how to use the LightSaber library.
I use it as a template for all my applications.

[Minimal]
The Minimal example shows you what you can achieve with the minimal amount of code:
 * automatically save GUI's state on program shutdown (form position on screen, checkbox state, etc)
 * automatically restore GUI's state on program startup.

[Simple]
Additional to the above example, this example has only a few lines of extra code:
 * Proper code initialization (in FormInitialize instead of FormCreate)
 * Create a desktop shortcut for your application 
 * Associate your application with a file type/file extension, so your application starts automatically when someone double-clicks that file type in Explorer
 * Shows the BetaTesterMode capabilities
 * Show strings in the program's caption while still always showing the program's name and version.
 
[Full] 
 Demonstrates even more of LightSaber's capabilities (but not even by far all):
 * Translate the GUI in multiple languages
 * Program news (Check website for news and display news) 
 * Program self update (check website for new versions)
 * User-selectable skins
 * Automatic logging
 * Automatically pop up the log on warnings and errors
 * Settings form 
 * Convert the program to paid Trial/Shareware (via Proteus Library) 
 * Minimize to system tray (via external library) 

Automatically restoring the GUI
A word of warning about this. In a well written program, the business logic should not be kept in the GUI. Any settings should not be read from the checkboxes of the GUI/form but from INI files or (much, much) better, binary objects. TfrmSettings.CreateFormModal is a good example of how to do this.

Therefore, automatically saving and restoring the GUI (checkboxes, radio boxes, etc.) is not fully recommended and though simple and (extremely) time-efficient and cool, should be done only for simple forms! 

This is not a problem of the LightSaber library - this has to do with proper application architecture. 

