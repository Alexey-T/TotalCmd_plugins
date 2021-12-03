  Caption:= MsgCaption(0250);
  btnOk.Caption:= MsgCaption(0105);
  btnCancel.Caption:= MsgCaption(0151);

  ListView1.Columns[0].Caption:= MsgCaption(0354);
  ListView1.Columns[1].Caption:= MsgCaption(0358);

  cLibTypeNames[fLister]:= S0040;
  cLibTypeNames[fPacker]:= S0041;
  cLibTypeNames[fFS]:=     S0042;
  cLibTypeNames[fContent]:= S0043;

  Label1.Caption:= Format(MsgCaption(0251), [cLibTypeNames[FOrderType]]);
  btnUp.Caption:= MsgCaption(0252);
  btnDown.Caption:= MsgCaption(0253);
