object frmEULA: TfrmEULA
  Left = 815
  Top = 300
  ActiveControl = btnOK
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'License agreement'
  ClientHeight = 436
  ClientWidth = 569
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object btnOK: TButton
    AlignWithMargins = True
    Left = 33
    Top = 385
    Width = 503
    Height = 44
    Hint = 'Press OK, Enter or Esc to agree with EULA.'
    Margins.Left = 33
    Margins.Top = 7
    Margins.Right = 33
    Margins.Bottom = 7
    Align = alBottom
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object mmoLicense: TMemo
    Left = 0
    Top = 0
    Width = 569
    Height = 378
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BorderStyle = bsNone
    Lines.Strings = (
      ''
      ''
      ''
      ''
      'License Agreement (v5.07)'
      ''
      ''
      ''
      ''
      ''
      ''
      
        'In this document the current software is referred as '#39'the produc' +
        't'#39', the computer where the software and the license is installed' +
        ' is referred as '#39'the computer'#39'.'
      
        'The developer of the product is referred as '#39'the developer'#39' or a' +
        's '#39'the producer'#39'. '
      ''
      
        'The developer is willing to LICENSE the product to you ONLY UPON' +
        ' THE CONDITION THAT YOU ACCEPT ALL OF THE TERMS CONTAINED IN THI' +
        'S BINARY CODE LICENSE AGREEMENT AND SUPPLEMENTAL LICENSE TERMS (' +
        'COLLECTIVELY "AGREEMENT"). '
      
        'PLEASE READ THE AGREEMENT CAREFULLY. BY INSTALLING THIS SOFTWARE' +
        ', YOU ACCEPT THE TERMS OF THE AGREEMENT. '
      
        'IF YOU ARE NOT WILLING TO BE BOUND BY ALL THE TERMS, PLEASE CLOS' +
        'E THIS PROGRAM NOW. '
      
        'RUNNING THIS SOFTWARE AT ANY TIME AND PLACE IMPLIES THAT YOU HAV' +
        'E READ AND AGREED WITH THE LICENSE CONDITIONS STATED HERE.'
      ''
      'GRANT OF LICENSE'
      
        'The product and documentation accompanying this License, whether' +
        ' on disk, in memory, or on any other media (the "Software"), are' +
        ' licensed to you by the producer.'
      'We retain title to the product and related documentation. '
      ''
      'PERMITTED USES AND RESTRICTIONS'
      
        'You may use the product according to the restrictions imposed by' +
        ' BETA MODE, TIME-LIMITED TRIAL USE, or REGULAR USE as described ' +
        'herein.'
      
        'You may not decompile, reverse engineer, disassemble, modify, re' +
        'nt, lease, loan, sublicense, distribute or create derivative wor' +
        'ks based upon the product in whole or part or transmit the produ' +
        'ct over a network or from one computer to another, or permit oth' +
        'ers to use the product through the Internet or similar mechanism' +
        's.'
      
        'Your rights under this License will terminate automatically with' +
        'out notice from us if you fail to comply with any term(s) of thi' +
        's License and compensation will be required. '
      
        'The product was not designed, tested and validated for medical u' +
        'se or other type of data processing that could result in loss/da' +
        'mage of human file or property.'
      ''
      'TIME-LIMITED TRIAL USE'
      
        'THE TIME-LIMITED TRIAL LICENSE MAY DISABLE CERTAIN FUNCTIONS OF ' +
        'THE SOFTWARE.'
      
        'If you are using the product under a time limited/trial license,' +
        ' you may use the product for evaluation and trial use purposes o' +
        'nly on a single computer until the Expiration Date.'
      
        'Upon trial expiration, you may not continue to use the product o' +
        'r to use the results for any purposes or to publish them without' +
        ' explicit permission. You are not allowed to bypass the trial pr' +
        'otection mechanism!'
      ''
      'LICENSE USAGE AND REDISTRIBUTION'
      
        'Use of the product in REGULAR MODE requires a valid License for ' +
        'each computer that the product is used on. For each License you ' +
        'have purchased from us or from an authorized distributor, you ma' +
        'y install the product on a single computer, and make one copy (C' +
        'D/DVD) of the product in machine-readable form for backup purpos' +
        'es.'
      ''
      
        'At any point in time, the total number of concurrent users of th' +
        'e product is limited to the number of Single User Licenses or Co' +
        'ncurrent User Licenses you purchased. For each License, you will' +
        ' be provided with an UnlockKey, which is valid for the original ' +
        'hardware configuration at the moment of installation.'
      ''
      
        'The license is not transmissible and is bound to a single standa' +
        'rd PC computer (not a server). Multiple users of the computer ca' +
        'n use the license but they cannot use the license simultaneously' +
        '.'
      
        'The license cannot be installed in shared (multi-terminal) syste' +
        'ms or virtual machines. Using the software via '#39'remote'#39' connecti' +
        'on to the computer is prohibited.'
      ''
      
        'You need to obtain EXPLICIT permission before redistributing/res' +
        'elling our products. The license in this case is sent directly t' +
        'o the end user, not to the reseller!'
      ''
      'LOST KEYS'
      
        'The unlock key we provide you are tagged with your name. If your' +
        ' key is leaves your computer (stolen due to negligence or if you' +
        ' specifically give it/borrow it to a 3rd party) and it become pu' +
        'blic, then it will be listed in our '#39'Stolen keys'#39' database.'
      
        'The product will refuse to work with a key listed in the '#39'Stolen' +
        ' keys'#39' database and we reserve the right to terminate the contra' +
        'ct with you. Financial compensation may be required in this case' +
        '.'
      
        'We reserve the right not to grant additional license keys if we ' +
        'suspect illegitimate use of software/key. '
      ''
      'UPDATES AND SUPPORT'
      
        'The purchase of a License includes a limited period of technical' +
        ' support. Minor updates may be granted for free. Upgrades to the' +
        ' next major version ARE NOT free.'
      ''
      'NON PAYMENTS AND DELAYS'
      
        'The recommended interval for completing the payment process is 2' +
        ' weeks. The maximum payment period is one month. Under no circum' +
        'stances the customer will be excused by delaying the payment for' +
        ' more than one month. In case you fail complete the payment in t' +
        'ime or you only submit a partial payment, we reserve the right t' +
        'o terminate the contract with you. You are not allowed to use th' +
        'e product until the full payment is sent otherwise we may ask fo' +
        'r interest or financial compensations calculated at a '#39'per day'#39' ' +
        'basis. More details are available on our web site.'
      
        'The following items may not be guaranteed for customers that unn' +
        'ecessarily delay the payment: support, unlock keys, updates, sof' +
        'tware downloads.'
      ''
      'COPYRIGHT'
      
        'All copyrights related to the product and any copies thereof are' +
        ' owned by the producer. No rights, except the right to use the s' +
        'oftware (as explained in this document), transfers to you when y' +
        'ou obtain a License. '
      
        'By using our software you allow us to list your name (or your or' +
        'ganization'#39's name) on '#39'Our customers'#39' page, unless you contact u' +
        's in written (emails are accepted) and clearly specify that you ' +
        'want to withhold from this.'
      ''
      'WARRANTY DISCLAIMER'
      
        'THE PRODUCER EXPRESSLY DISCLAIMS ANY WARRANTY FOR THE PRODUCT. T' +
        'HE PRODUCT AND ANY RELATED DOCUMENTATION IS PROVIDED "AS IS" WIT' +
        'HOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING,' +
        ' WITHOUT LIMITATION, THE IMPLIED WARRANTIES OR  MERCHANTABILITY,' +
        ' FITNESS FOR A PARTICULAR PURPOSE, OR NONINFRINGEMENT or THAT AN' +
        'Y INFORMATION ACCESSED BY THE PRODUCT WILL BE ACCURATE OR COMPLE' +
        'TE.'
      
        'THE ENTIRE RISK ARISING OUT OF THE USE OR PERFORMANCE OF THE PRO' +
        'DUCT REMAINS WITH YOU.'
      ''
      'DISCLAIMER OF DAMAGES'
      
        'TO THE EXTENT NOT PROHIBITED BY LAW, IN NO EVENT WILL THE PRODUC' +
        'ER OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DA' +
        'TA, OR FOR SPECIAL, INDIRECT, CONSEQUENTIAL, INCIDENTAL OR PUNIT' +
        'IVE DAMAGES, HOWEVER CAUSED REGARDLESS OF THE THEORY OF LIABILIT' +
        'Y, ARISING OUT OF OR RELATED TO THE USE OF OR INABILITY TO USE S' +
        'OFTWARE, EVEN IF THE PRODUCER HAS BEEN ADVISED OF THE POSSIBILIT' +
        'Y OF SUCH DAMAGES. '
      
        'In no event will producer'#39's liability to you, whether in contrac' +
        't, tort (including negligence), or otherwise, exceed the amount ' +
        'paid by you for the product under this Agreement. The foregoing ' +
        'limitations will apply even if the above stated warranty fails o' +
        'f its essential purpose. '
      ''
      'WEB SITE'
      
        'To function correctly the software may need to connect to our we' +
        'bsite (for example for checking updates, listing product news, d' +
        'ownloading additional resources (documents, applications), etc).'
      
        'By using the software you also agree with the current state of f' +
        'unctionality of our website. The site may not function correctly' +
        ' in some exotic browser or if you have certain plugins/addons in' +
        'stalled (such us ad-blockers).'
      
        'The functionality of the software can be limited when it cannot ' +
        'connect to the web site (for example you will not be announced w' +
        'hen a new update is available).'
      'We may use cookies on our website. '
      
        '                                                                ' +
        '                                                                ' +
        '                                                                ' +
        '       '
      'REFUND POLICY'
      
        'This product is distributed as Trial/Demo software, which implie' +
        's that prior to acquisition you tested the product and at moment' +
        ' of acquisition you are aware of the features provided within an' +
        'd the way how the product performs. On our web site we strongly ' +
        'encourage the customers to test our software before purchasing i' +
        't. Therefore, after the acquisition of the product, we guaranty ' +
        'NO refund! In case we accept to make a refund, taxes that we had' +
        ' to honor to 3rd parties (costs for CD, mailing, support, manual' +
        ' processing of forms and papers, partners and reseller royalties' +
        ', discounts, etc) are NOT refunded.'
      ''
      'TERMINATION'
      
        'We may terminate the right to use the product on notice to you (' +
        'which may be delivered electronically by the same means as the l' +
        'icense for the product is distributed), and you may terminate th' +
        'is License at any time by uninstalling, erasing and destroying y' +
        'our copy of the product and of related items (data, license keys' +
        ', user manual, etc). Upon termination of this License, you must ' +
        'discontinue all use of the product. No refunds will be granted i' +
        'n case of contract termination.'
      ''
      'BETA MODE & BUG REPORTS'
      
        'In Pre-Release and Beta mode there is no warranty AT ALL about t' +
        'he quality of the behavior and data/results produced by the prod' +
        'uct. These modes are intended only to show you the capabilities ' +
        'of the product. In this mode the product might contains undocume' +
        'nted bugs. We assume *NO* responsibilities at all for this mode.' +
        ' You are not allowed to use the results of a beta version.'
      
        'In case of program malfunction (internal errors, crashes, etc) t' +
        'he user can send through the program malfunction-related data (p' +
        'rogram/computer status data and screenshot at the moment of malf' +
        'unction) to us. By installing and using the program you agree wi' +
        'th this. No personal files will be sent to us. However, the repo' +
        'rt may contain user-identifiable data. You have the choose to su' +
        'bmit the bug report to us or not. The bug report you send to us ' +
        'is deleted after the bug has been investigated, classified and s' +
        'olved. We may notify the customers when the bug has been fixed. ' +
        'Bug reports are necessary in order to provide ever increasing qu' +
        'ality of the product. We encourage the user to let the program t' +
        'o send the bug reports to us. However, we don'#39't enforce it. The ' +
        'user may opt not to send the bug reports to us, however, we migh' +
        't not be able to offer technical support when the malfunction-re' +
        'lated data is insufficient (the bug reports are not provided).'
      ''
      'GENERAL'
      
        'This License will be construed under the laws of the state/count' +
        'ry where the product was developed, except for that body of law ' +
        'dealing with conflicts of law.'
      
        'If any provision of this License shall be held by a court of com' +
        'petent jurisdiction to be contrary to law that provision will be' +
        ' enforced to the maximum extent permissible and the remaining pr' +
        'ovisions of this License will remain in full force and effect. A' +
        'ny conflict shall be solved in the above-mentioned country.'
      
        'No other document created by the buyer (for example, a '#39'Purchase' +
        ' order terms & conditions'#39'), electronic or  on paper, signed or ' +
        'unsigned, can override this one!'
      ''
      'COMPLETE AGREEMENT'
      
        'This License constitutes the agreement between the parties with ' +
        'respect to the use of the product and supersedes all prior or co' +
        'ntemporaneous understandings regarding such subject matter. No a' +
        'mendment to or modification of this License will be binding unle' +
        'ss in writing and signed by us. '
      
        'Updates of this license will be listed on our web site. Addition' +
        'al terms and conditions may be displayed on our web site. '
      ''
      'Note: '
      
        '  Some jurisdictions do not allow the exclusion of incidental or' +
        ' consequential damages, so some of the terms above may not be ap' +
        'plicable to you.'
      
        '  This license also applies to the tools/resources accompanying ' +
        'this software product.'
      '  '
      'By using the software you implicitly agree with this EULA!')
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
    WordWrap = False
  end
end
