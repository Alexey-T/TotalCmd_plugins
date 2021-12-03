procedure CheckCommandLine;
var
  S: string;
begin
  if ParamCount > 0 then
    begin
    //First we need to check are TC configuration files specified
    if not SettingsOK then
      Halt;

    //OK, handle command line
    S:= ParamStr(1);

    if SIEqual(S, '/restart') then
      begin
      RestartTC;
      Halt
      end;

    if SIEqual(S, '/orderWLX') then
      begin
      ShowOrderDialog(fLister);
      Halt;
      end;

    if SIEqual(S, '/orderWDX') then
      begin
      ShowOrderDialog(fContent);
      Halt;
      end;

    if SIEqual(S, '/orderWCX') then
      begin
      ShowOrderDialog(fPacker);
      Halt;
      end;

    if SIEqual(S, '/Uninstall') then
      if ParamCount > 1 then
        begin
        UninstallPluginFilename(ParamStr(2));
        Halt;
        end;

    //Unknown parameter specified
    if Pos('/', S) = 1 then
      begin
      MsgError(S0060);
      Halt;
      end;

    //No special parameters passed, do the install
    InstallPluginWithConfirmation(S);
    Halt;
    end;
end;
