TestInsight 1.2.0.8
Copyright (c) 2014-2025 Stefan Glienke - All rights reserved 

Please report issues on http://bitbucket.org/sglienke/testinsight/issues

HOW TO GET STARTED:

After installation you can access TestInsight in the IDE by selecting View->TestInsight Explorer.

In order to make TestInsight work with your project it needs to have the TESTINSIGHT compiler directive 
defined in the project. You can do that quickly with the context menu in the project manager.

In your application you need to use the TestInsight.<framework> unit for the framework you are using.
Just call the RunRegisteredTests routine or manually register the Listener/Logger to your test framework.