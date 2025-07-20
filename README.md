# Delphi-LightSaber


![Delphi Library](https://raw.githubusercontent.com/GodModeUser/Delphi-LightSaber/main/Docs/Lightsaber%20logo.png)

**Description**   

This repository contains a group of general-purpose Delphi libraries, that make your life easier.
The library wants to be a lightweight replacement for the mammoth Jedi library.  
Simple, crystal clear, non-obfuscated, fully commented and curated code.  
No (direct) external dependencies.   
More than 15 years of development have been put in this library.     

**What you can do with this library**   

There are a gazillion of cool things this library can do for you. Here are only a few examples, and all from the Core/Base library (I am not even touching the higher-level libraries):  
 - Save your GUI state (checkboxes, radio buttons, edit boxes, etc.) to disk and then restore it when the application starts. All this with only 4 lines of code.  
 - Automatically translate your GUI to multiple languages via DeepL (no lines of code).  
 - Logging - Send color-coded messages to a log. The log will automatically pop up on errors, so the user can take action.  
 - AppData - A dramatically improved TApplication object that gives you lots and lots and lots of goodies such as "RunningHome", "RunningFirstTime", "BetaTesterMode", "CurrentUserDir", "LastUsedFolder"  
 - "SingleInstance" (prevent your program from running as multiple instances in a computer and pass the command line parameters to the already running instance)  
 - Uninstaller - The application writes itself in Windows "Add remove programs" so the user can uninstall it.  
 - App control: Restore, Restart, SelfDelete (useful for the uninstaller), RunSelfAtWinStartUp, HideFromTaskbar...  
 - etc etc etc  

Most of this functionality can be achieved with maximum 2-3 lines of code. Some of them with zero lines of code!  
  
Click the 'Watch' button if you want to get notified about updates.    
[More libraries like this](https://gabrielmoraru.com/my-delphi-code)  

**Documentation**

Emergency documentation update 2025:

The library had an MASSIVE MASSIVE update in the last months because I try to make it cross-platform.
The part of the documentation about the file naming and structure is now 100% obsolete. 
Once the porting is over I will update also the documentation. 
Now you need to install:
 * LightCore.dpk - if you want to compile only the RTL part of the library 
 * LightSaberFMX.groupproj - for the RTL/FMX part of the library 
 * LightSaberVCL.groupproj - for the RTL/VCL part of the library 

If the compiler complain that there are some some missing libraries like 
GR32, CcrExif, ThirdPartyLibs, janFX, Jpeg2000: These are 3rd party libraries. You will have to download them from Internet OR simply ignore them. My LightSaber can include or exclude those 3rd party libraries.
Flipp the following a compiler directives in LightVclGraphics.dpk, Project Option:
{$UNDEFINE DEBUG}
{$UNDEFINE CCRExif}
{$UNDEFINE Jpg2000}
{$UNDEFINE FastJpg}

 ----
 
The code is nicely documented. Each method has a comment on top of it.  
Some units have FULL documentation at the top and code/usage examples.  
Complex classes have testing code that can be used also as demo (example of usage) programs. Check the "\LightSaber\Demo\" folder.  
I also started a new video channel. I will post a new video every time this library collects 100 stars.   
https://www.youtube.com/watch?v=a9nZiDXYmIo  

All documentation is in English.

**Programming standards**

This library follows strictly the following programming standards:
 - Zero tolerance for global variables  
 - Zero tolerance for compiler hints and warnings  
 - Zero tolerance for swallowed exceptions  
 - Zero tolerance for memory leaks  
 - Consistent code formatting - most of my formatting respects the Embarcadero recommendations. A noticeable exception is if/then/else.
 - Consistent file naming.

The library uses modern packaging techniques and its nicely split into layers and grouped under a master file "LightSaber.groupproj".  
By the grace of the new $ (Auto) compiler directive, the madness of "one package per Delphi version" is gone now.  
(If you use an older Delphi edition that does not support the $(Auto) directive, just delete it and recompile.)  

**Road Map**  

The LightSaber Internet, Graphics, LightSaber Visual Controls was added in 2023, as promised.
The Proteus library will be added as soon as my free time allows it.   

**Pricing**  

This library is freeware (see included copyright notice). 
I don't ask for money, not even donations for this library, but you can help me by spreading the word.   

This library will be expanded if it gets enough stars:  
 - Click the 'Star' button (top-right corner) if you like this library.   
 - More than 100 files are waiting to be cured and added.

Note: The library cannot be used in Russia! If you are from Russia and want to use this library, we can arrange a price per PAS unit.  

**Installation**  

Double-click the LightSaber.groupproj to load it in Delphi.
In Project Manager, right-click on the group (top) and select "Build all" in the popup menu. If you want to install also the visual components, right click on them and choose "Install".   
That's it.
_________________

**Structure** 

This repository contains the following libraries: 
 - Core
 - AppData + Log + FormTranslator
 - Common  
 - Internet  
 - Translator  
 - Graphics  
 - Visual Controls  
 
Each library depends on the previous one, in the order specified above.  
LightSaber Core does not depend on any other library.   
Therefore, if you want the Log library, you need to compile also the Core library.
Your code could use directly the bottom library (Core) or higher libraries in the hierarchy, depending on your requirements:

![](https://raw.githubusercontent.com/GodModeUser/Delphi-LightSaber/main/Docs/Library%20architecture.png)

If you only need the only one library (for example, the Core library), you can of course delete the rest of the libraries.
More about my libraries [here](https://gabrielmoraru.com/publications-citations/).


**Filename convention**  

The following filename convention is used in LightSaber libraries:  
- The first letter (c) stands for 'cubic' (the original name of the library)
- The second letter is as follows:
   - 'c' stands for 'core'.  (All other top libraries are based on the Core library.)
   - 'b' stands for 'base'. 
   - 'v'-> visual component. 
   - 'Graph'-> graphic library.
   - 'l' -> Log library.
   - 'i' -> Internet library.
   - 'm' -> Common library.

Example:   
- ccBinary.pas  (Cubic core library)
- cvMemo.pas    (Cubic visual component)
- cGraphFX.pas  (Cubic graphic library) 
- clVisLog.pas  (Cubic log library)  
- ciEmailSender.pas (Cubic internet library)  
- cmPowerUtils.pas  (Cubic common library)  
- ciEmailSender.pas (Cubic internet library)  
- cmPowerUtils.pas  (Cubic common library)  

**Example of units** (showing only units in the Core library)

**ccCore.pas**  
  Over 200 functions for:  
- String manipulation (string conversions, sub-string detection, word manipulation, cut, copy, split, wrap, etc.)  
- Programmer's helper  
- Form manipulation  
- Advanced/easy message boxes  
- DateTime utilities  
- etc etc etc etc etc etc etc 
    
**ccIO.pas**  
  Super useful functions for file/folder/disk manipulation:  
- Copy files   
- File/Folder Exists    
- Get special Windows folders (My Documents, etc.)  
- Prompt the user to select a file/folder  
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
- Get application's %AppData% folder (the folder where you save temporary, app-related and ini files)
- Get application's command line parameters
- Detect if the application is running for the first time on a computer.
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
     Allows you to directly read/write bytes, cardinals, words, integers, and strings to a (binary) files.  
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
Use it to get the Windows version.  
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
- Functions for easily accessing the application's default INI file.  

Setup:  
     Before using it you must set the ccAppData.AppName global var.  
     The class will use that name to automatically determine the INI file name/path which is %AppData%\AppName.Ini.  
     Example: If the AppName is set to "DelphiLightSaber" the ini file will be  "c:\Users\UserName\AppData\Roaming\DelphiLightSaber\DelphiLightSaber.ini" 
 
 
**And... the cherry on the top of the cake was left at the end!**

Resume application's GUI state via **ccINIFileVCL.pas** 

Do you have applications with forms with lots of controls (like checkboxes/radio buttons) and you want to save its status to disk on shutdown and resume exactly from where you left off at application startup with just one function call?  
  Use SaveForm/LoadForm.  

Example:   
- Call SaveForm(MySettingsForm) in TMySettingsForm.OnDestroy     
- Call LoadForm(MySettingsForm) after the creation of TMySettingsForm     

A full demo app that demonstrates how to save/load the GUI with just two lines of code can be found here: [github.com/GodModeUser/Dephi-LightSaber-GUI_AutoSave ](https://github.com/GodModeUser/Dephi-LightSaber-GUI_AutoSave)

_________________

**External dependencies**

Some parts (especially the graphic part) of LightSaber depend on these external libraries:  

- https://github.com/esmondb/ccr-exif/    
- https://github.com/galfar/PasJpeg2000    
- http://www.marktg.com/jpegdec/  

HOWEVER, those dependencies are disabled by default, so you don't need to install any extra libraries in order to use the LightSaber. But in this case, some functionality (Exif support for JPG images, Fast Jpeg decoder, and support for Jpeg2000) of LightSaber will not be available, but the impact will be minimal. 

To enable the support for the above-mentioned features, install the libraries (see links below) and then add these "defines" to your project's settings: CCRExif;FastJpg;Jpg2000.     
The "Conditional defines" field is located in Project Options: Right-click your project, and choose "Project options -> Delphi compiler -> All configurations -> Conditional defines".   

_________________

[More like this](https://gabrielmoraru.com/my-delphi-code) 
