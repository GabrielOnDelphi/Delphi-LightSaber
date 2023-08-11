# LightSaber


**Description**   

This repository contains a group of general-purpose Delphi libraries, that make your life easier.
The library also wants to be a lightweight replacement for the mammoth Delphi Jedi library.  
Simple, crystal clear, non-obfuscated, fully commented and curated code. 
No external dependencies. 

More than 15 years of development have been put in this library.   
Click the 'Watch' button if you want to get notified about updates.  

_________________

**Structure** 

This repository contains the following libraries: 
 - Core
 - Log
 - Common
 - Internet
 - Translator 
 
Each library depends on the previous one, in the order specified above.  
LightSaber Core does not depend on any other library.   
Therefore, if you want the Log library, you need to compile also the Core library.
Your code could use directly the bottom library (Core) or higher libraries in the hierarchy, depending on your requirements:

![](https://raw.githubusercontent.com/GodModeUser/Delphi-LightSaber/main/Docs/Library%20architecture.png)

If you only need the only one library (for example, the Core library), you can of course delete the rest of the libraries.
_________________

**Filename convention**  

The following filename convention is used in LightSaber libraries:  
- The first letter (c) stands for 'cubic' (the orginal name of the library)
- The second letter is as follows:
- 'c' stands for 'core'.  (All other top libraries are based on the Core library)
- 'v'-> visual component. 
- 'Graph'-> graphic library.
- 'l' -> Log library.
- 'i' -> Internet library.
- 'm' -> Common library.

Example:   
- ccBinary.pas  (Cubic core library)
- cvMemo.pas    (Cubic visual component)
- cGraphFX.pas  (Cubic graphic library) 
- clVisLog.pas (Cubic log library)  
- ciEmailSender.pas (Cubic internet library)  
- cmPowerUtils.pas (Cubic common library)  
- ciEmailSender.pas (Cubic internet library)  
- cmPowerUtils.pas (Cubic common library)  

_________________

**Road Map**  

The LightSaber Graphics, LightSaber Visual Controls and Proteus libraries will be added as soon as my free time allows it.   

**This library will be expanded if it gets enough Stars.**   
Click the 'Star' button (top-right corner) if you like this library.  
Over 100 files are waiting to be cured and added: graphichs, simple encryption, internet functions (including file download routines), HTML manipulation, image manipulation, registry, math and LOTS of visual components!


This library is freeware (see included copyright notice). The library cannot be used in Russia!

_________________

**Installation**  

Double click the LightSaber.groupproj to load it in Delphi.
In Project Manager, right click on the group (top) and select "Build all" in the popup menu.

_________________

**Files in CoreLib**

**ccCore.pas**  
  Over 200 functions for:  
- String manipulation (string conversions, sub-string detection, word manipulation, cut, copy, split, wrap, etc)  
- Programmer's helper  
- Form manipulation  
- Advanced/easy message boxes  
- DateTime utilities  
- etc etc etc etc etc etc etc 
    
    
**ccIO.pas**  
  Super useful functions for file/folder/disk manipulation:  
- Copy files   
- File/Folder Exists    
- Get special Windows folders (My Documents, etc)  
- Prompt user to select a file/folder  
- List specified files (.jpg for ex) in a folder and all its sub-folders  
- Increment the numbers in a filename (good for incremental backups)  
- Append strings to file name  
- Read text from files to a string variable  
- Compare files  
- Merge files  
- Sort lines in a file  
- Drive manipulation (IsDiskInDrive, etc)    
- etc  
     
**ccAppData.pas**  
Application-wide functions:
- Get application's appdata folder (the folder where you save temporary, app-related and ini files)
- Get application's command line parameters
- Detect if the application is running for the firs this in a computer
- Application self-restart
- Application self-delete
- etc
     
**ccStreamBuff.pas**  
Extends TBufferedFileStream.  
This class adds new functionality that does not exist in Delphi's original stream classes:  
- Read/WriteBoolean  
- Read/WriteString (Ansi/Unicode)  
- Read/WriteInteger  
- Read/WriteCardinal  
- Read/WriteDate  
- Read/Write mac files (inverted byte endianness)  
- etc  
It may be used as a drop-in replacement for TFileStream.  
  
     
**ccStreamFile.pas**  
     Expansion class for Delphi classical TFileStream.   
     Allows you to directly read/write bytes, cardinals, words, integers, strings to a (binary) files.  
     Now replaced by ccStreamBuff.
     
**ccBinary.pas**  
- String to hex, hex to string conversions (and many others)  
- Binary numbers (endianness) swapping  
- Data serialization  
- Bit manipulation (set bit, etc)  
- Reverse bits  
- Endianess
- etc   

**ccWinVersion.pas**  
This library expands the TOSVersion.  
Use it to get Windows version.  
Example of functions:    
- IsWindowsXP  
- IsWindowsXPUp  
- IsWindowsVista    
- IsWindowsVistaUp  
- IsWindows7  
- IsWindows7Up  
- IsWindows8  
- IsWindows8Up  
- IsWindows10  
- etc   

**ccINIFile**  
Features:  
- Extends the capabilities of TIniFile  
- Functions for easily accessing application's default INI file.  

Setup:  
     Before using it you must set the ccAppData.AppName global var.  
     The class will use that name to automatically determine the INI file name/path which is %AppData%\AppName.Ini.  
     Example: If the AppName is set to "DelphiLightSaber" the ini file will be  "c:\Users\UserName\AppData\Roaming\DelphiLightSaber\DelphiLightSaber.ini" 
 
 
**And... the cherry on the top of the cake was left at the end!**

Resume application's GUI state via **ccINIFileVCL.pas** 

Do you have applications with forms with lots of controls (like checkboxes/radiobuttons) and you want to save its status to disk on shutdown and resume exaclty from where you left on application startup with just one function call?  
  Use SaveForm/LoadForm.  

Example:   
- Call SaveForm(MySettingsForm) in TMySettingsForm.OnDestroy     
- Call LoadForm(MySettingsForm) after the creation of TMySettingsForm     

A full demo app that demonstrates how to save/load the GUI with just two lines of code can be found here: [github.com/GodModeUser/Dephi-LightSaber-GUI_AutoSave ](https://github.com/GodModeUser/Dephi-LightSaber-GUI_AutoSave)



_____

