CCR Exif v1.5.1


What it is 
- A small class library to edit, create and delete Exif and IPTC metadata in JPEG files,
  Exif being the sort of metadata that most digital cameras write.

Author
- Chris Rolliston.

Licence
- MPL 1.1 (text at http://www.mozilla.org/MPL/MPL-1.1.html).

Features
- Exif parsing is 100% pure Delphi code ÅEdoesnít use (say) LibExif or LibTiff, or an
  operating system graphics API.
- Compiles for all three XE2 targets (Win32, Win64 and OS X), together with older Win32
  versions going back to Delphi 2006 (Delphi 2007 or above preferred however).
- Reads and writes both small- and big-endian data.
- Surfaces both standard Exif and Windows Explorer tags, and provides access to the tags
  of some maker note types too.
- Doesnít corrupt internal maker note offsets when data is rewritten, and takes account
  of the Microsoft-defined OffsetSchema tag.
- Can optional write XMP data as per the XMP Exif schema.
- Includes an IPTC reader/writer class as well.

Find this code useful?
- Buy my book! Details at http://delphifoundations.com/