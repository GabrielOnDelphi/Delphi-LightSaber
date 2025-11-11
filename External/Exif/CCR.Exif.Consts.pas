{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.3                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.Consts.pas.                                            }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2014 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit CCR.Exif.Consts;

interface

const
  CCRExifVersion = '1.5.3';

resourcestring
  SInvalidHexString = 'Invalid hex string (%s)';
  SStreamIsReadOnly = 'Stream is read-only';
  SUnsupportedGraphicFormat = 'Unsupported graphic format';
  SInvalidJPEGHeader = 'JPEG header is not valid';
  SInvalidPSDHeader = 'Photoshop (PSD) file header is not valid';
  SFileIsNotAValidJPEG = '"%s" is not a valid JPEG file';
  SInvalidTiffData = 'Invalid TIFF data';
  SInvalidOffsetTag = 'Tag does not specify an offset';
  SInvalidExifData = 'Malformed EXIF data';
  SAsciiValueCannotBeArray = 'An ASCII tag cannot be an array';
  SUndefinedValueMustBeBeArray = 'An undefined tag must be an array';
  SInvalidFraction = '''%s'' is not a valid fraction';
  STagAlreadyExists = 'Tag with ID of "%d" already exists in section';
  SNoFileOpenError = 'No file is open';
  SIllegalEditOfExifData = 'Illegal attempt to edit the Exif data in such ' +
    'a way that it would change the file structure';
  STagCanContainOnlyASCII = 'Tag may contain only ASCII string data';

  SInvalidMakerNoteFormat = 'Invalid MakerNote format';

  SCannotRewriteOldStyleTiffJPEG = 'Cannot rewrite old style TIFF-JPEG'; 

  SInvalidXMPPacket = 'XMP packet is not valid';
  SSubPropertiesMustBeNamed = 'Sub-properties must be named';
  SSubPropertiesNotSupported = 'Property type does not support sub-properties';
  SCannotWriteSingleValueToStructureProperty = 'Cannot assign a single value to a structure property';
  SPreferredPrefixMustBeSet = 'The schema''s PreferredPrefix property must be set before a new item can be added';

  SInvalidAdobeSegment = 'Invalid Adobe metadata segment';
  SInvalidIPTCTagSizeField = 'Invalid IPTC tag size field (%d)';

  SAboveSeaLevelValue = '%s above sea level';
  SBelowSeaLevelValue = '%s below sea level';
  STrueNorthValue = '%s° true north';
  SMagneticNorthValue = '%s° magnetic north';

implementation

end.
