  Caption:= Format(S0050, [SVersion]);
  Application.Title:= Caption;

  cLibTypeNames[fLister]:= S0040;
  cLibTypeNames[fPacker]:= S0041;
  cLibTypeNames[fFS]:=     S0042;
  cLibTypeNames[fContent]:= S0043;

  chkQS1.Caption:= MsgCaption(0120);
  chkQS2.Caption:= MsgCaption(0121);

  labTCExe.Caption:= MsgCaption(0051);
  labTCVersion.Caption:= MsgCaption(0052);
  labTCState.Caption:= MsgCaption(0053);
  labTCIni.Caption:= MsgCaption(0054);
  btnBrowseExe.Hint:= MsgCaption(0055);
  edTCExe.Hint:= MsgCaption(0056);
  edTCIni.Hint:= MsgCaption(0057);
  btnBrowseIni.Hint:= MsgCaption(0058);
  btnTCRun.Hint:= MsgCaption(0059);
  btnTCRun.Caption:= MsgCaption(0060);
  btnTCRestart.Hint:= MsgCaption(0061);
  btnTCRestart.Caption:= MsgCaption(0062);
  btnAdd.Hint:= MsgCaption(0063);
  btnAddMenu.Hint:= btnAdd.Hint;
  btnAdd.Caption:= MsgCaption(0064);
  btnOptions.Hint:= MsgCaption(0065);
  btnOptions.Caption:= MsgCaption(0066);
  btnClose.Hint:= MsgCaption(0069);
  btnClose.Caption:= MsgCaption(0070);
  btnEditIni.Caption:= MsgCaption(0112);
  btnEditIni.Hint:= MsgCaption(0113);

  btnDisable.Hint:= MsgCaption(0071);
  btnDisable.Caption:= MsgCaption(0085);
  btnEnable.Hint:= MsgCaption(0071);
  btnEnable.Caption:= MsgCaption(0086);
  btnRemove.Hint:= MsgCaption(0073);
  btnRemove.Caption:= MsgCaption(0074);
  btnRename.Hint:= MsgCaption(0075);
  btnRename.Caption:= MsgCaption(0076);

  OpenDialogExe.Title:= MsgCaption(0055);
  OpenDialogIni.Title:= MsgCaption(0058);
  OpenDialogPlugins.Title:= MsgCaption(0063);
  OpenDialogZip.Title:= MsgCaption(0063);
  OpenDialogDll.Title:= MsgCaption(0063);
  TabControl1.Hint:= MsgCaption(0088);

  btnConfigure.Caption:= MsgCaption(0081);
  btnConfigure.Hint:= MsgCaption(0082);
  btnOrder.Caption:= MsgCaption(0083);
  btnOrder.Hint:= MsgCaption(0084);
  btnOrderMenu.Hint:= btnOrder.Hint;
  btnTweak.Caption:= MsgCaption(0093);
  btnTweak.Hint:= MsgCaption(0094);
  mnuOptOptions.Caption:= btnOptions.Caption + '...';
  mnuOptReport.Caption:= MsgCaption(0114) + '...';
  mnuOptAbout.Caption:= MsgCaption(0068);

  mnuOrderLister.Caption:= Format(MsgCaption(0117), [S0040]);
  mnuOrderPacker.Caption:= Format(MsgCaption(0117), [S0041]);
  mnuOrderContent.Caption:= Format(MsgCaption(0117), [S0043]);

  with TabControl1 do
    begin
    Tabs[0]:= S0044; //All
    Tabs[1]:= S0040;
    Tabs[2]:= S0041;
    Tabs[3]:= S0042;
    Tabs[4]:= S0043;
    Tabs[5]:= S0045; //QS
    end;

  //context menus
  mnuDisable.Caption:= btnDisable.Caption;
  mnuEnable.Caption:= btnEnable.Caption;
  mnuUninstall.Caption:= btnRemove.Caption;
  mnuRename.Caption:= btnRename.Caption+'...';
  mnuTweak.Caption:= btnTweak.Caption+'...';
  mnuConfigure.Caption:= btnConfigure.Caption+'...';
  mnuBrowse.Caption:= MsgCaption(0089);
  mnuInstall.Caption:= MsgCaption(0064)+'...';
  mnuInstallFolder.Caption:= MsgCaption(0095);
  mnuInstallZip.Caption:= MsgCaption(0091);
  mnuOrder.Caption:= MsgCaption(0083)+'...';
  mnuRefresh.Caption:= MsgCaption(0087);
  mnuFilter.Caption:= MsgCaption(0116)+'...';
  mnuProperties.Caption:= MsgCaption(0077);

  mnu2Install.Caption:= mnuInstall.Caption;
  mnu2InstallFolder.Caption:= mnuInstallFolder.Caption;
  mnu2InstallZip.Caption:= mnuInstallZip.Caption;
