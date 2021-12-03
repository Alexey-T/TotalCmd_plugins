{ ##
  @FILE                     PJMenuSpeedButtons.pas
  @COMMENTS                 Source code for components
  @PROJECT_NAME             Menu-related speed buttons
  @PROJECT_DESC             A set of speed button derived components that either
                            display associated menus or form button / menu
                            button groups.
  @AUTHOR                   Peter Johnson, LLANARTH, Ceredigion, Wales, UK
  @OWNER                    delphiDabbler
  @EMAIL                    peter.johnson@openlink.org
  @WEBSITE                  http://www.delphidabbler.com/
  @COPYRIGHT                © Peter D Johnson, 2001-2003.
  @LEGAL_NOTICE             These components and source code are placed in the
                            public domain. They may be freely copied and
                            circulated on a not-for-profit basis providing that
                            the code is unmodified and this notice and
                            information about the author and his copyright
                            remains attached to the source code.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 17/03/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.0.1
      @DATE                 11/09/2003
      @COMMENTS             Changed component palette from "PJ Stuff" to
                            "DelphiDabbler".
    )
  )
}


unit PJMenuSpeedButtons;


interface


uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  Menus;


const
  {Custom messages processsed by components in this unit}
  PJM_LMOUSEDOWN      = WM_USER + 1;
  PJM_LMOUSEMOVE      = WM_USER + 2;
  PJM_LMOUSEUP        = WM_USER + 3;
  PJM_CAPTURECONTROL  = WM_USER + 4;
  PJM_ATTACH          = WM_USER + 5;

type

  {
  TPJMenuPlacing:
    Where a menu is placed relative to a menu button.
  }
  TPJMenuPlacing = (
    mpTopLeft,      // top-left of menu in same place as top-left of button
    mpBottomLeft,   // menu appears below button, aligned to button left
    mpTopRight,     // menu appear to right of button, aligned to button top
    mpBottomRight   // top-left of menu adjacent to bottom-right of button
  );


  {
  TPJUngroupedSpeedButton:
    Class of speed button which is never grouped with others: GroupIndex
    property is always 0.

    Inheritance: TPJUngroupedSpeedButton => [TSpeedButton]
  }
  TPJUngroupedSpeedButton = class(TSpeedButton)
  private // properties
    procedure SetGroupIndex(const Value: Integer);
    function GetGroupIndex: Integer;
  published
    { Redefined property inherited from base class}
    property GroupIndex: Integer
      read GetGroupIndex write SetGroupIndex default 0;
      {This property is effectively disabled: it can only take on value 0 to
      prevent this button being part of a radio group with other buttons, which
      makes no sense}
  end;


  {
  TPJCustomMenuSpeedButton:
    A speed button that has ability to display a linked popup menu. This base
    class defines but does not publish new properties. Descendent classes may
    publish required properties.

    Inherited: TPJCustomMenuSpeedButton => TPJUngroupedSpeedButton
      => [TSpeedButton]
  }
  TPJCustomMenuSpeedButton = class(TPJUngroupedSpeedButton)
  private // properties
    fReleaseButton: Boolean;
    fActiveMenu: TPopupMenu;
    fMenuPlacing: TPJMenuPlacing;
    procedure SetActiveMenu(const Value: TPopupMenu);
  private // other
    fInhibitClick: Boolean;
      {When this flag is true the Click method does nothing: used to prevent
      OnClick event from firing}
  protected // new properties
    property ActiveMenu: TPopupMenu
      read fActiveMenu write SetActiveMenu;
      {Reference to the menu that is popped up by this button}
    property ReleaseButton: Boolean
      read fReleaseButton write fReleaseButton default False;
      {When false the speed button remains depressed while menu is displayed and
      released only when menu is closed. When true the speed button is
      immediately returned to its "up" state as soon as mouse button is
      released}
    property MenuPlacing: TPJMenuPlacing
      read fMenuPlacing write fMenuPlacing default mpBottomLeft;
      {Determines where the top left of the popup menu is placed relative to the
      speed button}
  protected // other
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
      {Method called when user releases mouse button after clicking speed
      button: constructs and then pops-up colour menu. Also ensures that OnClick
      event is fired just before menu appears}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
      {Resets reference to ActiveMenu to nil if the associated popup menu
      component is being freed}
    procedure DisplayMenu(Pos: TPoint); virtual;
      {Pops up the atached menu}
    function PopupPosition: TPoint; virtual;
      {Returns the point, in screen coordinates, where the popup menu is to be
      displayed}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: sets default values}
    procedure Click; override;
      {Override of method that triggers OnClick event and / or Action's
      OnExecute event. The events may be inhibited in some circumstances to
      ensure that they are fired immediately before menu appears}
  end;


  {
  TPJMenuSpeedButton:
    A speed button that has ability to display a linked popup menu - publishes
    new properties defined in TPJCustomMenuSpeedButton.

    Inherited: TPJMenuSpeedButton => TPJCustomMenuSpeedButton
      => TPJUngroupedSpeedButton => [TSpeedButton]
  }
  TPJMenuSpeedButton = class(TPJCustomMenuSpeedButton)
  published
    { Newly published inherited properties }
    property ActiveMenu;
    property ReleaseButton;
    property MenuPlacing;
  end;

  TPJLinkedSpeedButton = class;


  {
  TPJLinkedMenuSpeedButton:
    A popup menu displaying speed button that can be associated and work in
    partnership with a master speed button. This button acts as an extension of
    master button when mastrer button is activated or clicked, but acts
    independently when this button itself is clicked.

    Inherited: TPJLinkedMenuSpeedButton => TPJCustomMenuSpeedButton
      => TPJUngroupedSpeedButton => [TSpeedButton]
  }
  TPJLinkedMenuSpeedButton = class(TPJCustomMenuSpeedButton)
  private // properties
    fMasterButton: TPJLinkedSpeedButton;
    fUseDefaultGlyph: Boolean;
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    procedure SetUseDefaultGlyph(const Value: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    function StoreGlyph: Boolean;   // storage specifier
  private // other
    {Fields that store references to mouse event handlers when mouse events are
    inhibited}
    fMouseMoveEventStore: TMouseMoveEvent;
    fMouseDownEventStore: TMouseEvent;
    fMouseUpEventStore: TMouseEvent;
    fMouseClickEventStore: TNotifyEvent;
    fMouseDblClickEventStore: TNotifyEvent;
    {Other fields}
    fControlCaptured: Boolean;
      {Flag that, when true, indicates that this control's mouse events are
      being artificially controlled by master speed button and should not be
      fired}
    fGlyphStyle: Integer;
      {Type of glyph currently assigned: required when loading from form file}
    procedure EnableMouseEvents(Flag: Boolean);
      {Disables or restores this buttons mouse event handler according to flag}
    procedure SetControlCaptured(Flag: Boolean);
      {Sets flag that shows if control is captured (ie mouse events are taken
      over by any associated master button). Disable/enables mouse events
      depending on whether captured}
  protected
    { Overrides }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
      {Override of notification method to set master button to nil when it is
      destroyed}
    procedure Loaded; override;
      {Override of loaded method to check if we must destroy the initial default
      glyph: in cases where no glyph is required}
    procedure DisplayMenu(Pos: TPoint); override;
      {Override of method that displays menu which inhibits display of menu when
      control's mouse up event is inhibited}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      {CM_MOUSEENTER mesasge handler: acts on event and passes on to any
      attached master button. Message passed on is specially cutomised to
      prevent infinite recursion. This handler is required to switch on
      highlighting on two associated buttons at same time}
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      {CM_MOUSELEAVE mesasge handler: acts on event and passes on to any
      attached master button. Message passed on is specially cutomised to
      prevent infinite recursion. This handler is required to switch off
      highlighting on two associated buttons at same time}
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {CM_ENABLEDCHANGED message handler: if any master button's enabled
      property is false then this button's state must remain false}
    { New methods }
    procedure LoadDefGlyph; virtual;
      {Loads the default glyph from resources into the Glyph property}
    procedure GlyphChange(Sender: TObject); virtual;
      {Handles event triggered when Glyph property changes: assumes that the
      glyph that has been set is user provided. If this is not the case then
      other code changes this value}
    procedure PJMLMouseDown(var Msg: TMessage); message PJM_LMOUSEDOWN;
      {Traps custom PJM_LMOUSEDOWN message, which calls MouseDown event with
      given info. Can be used to simulate mouse down events on and off the
      button to force button down, even when mouse is over any attached mastrer
      button. Only works when control's mouse events are captured by master
      button}
    procedure PJMLMouseUp(var Msg: TMessage); message PJM_LMOUSEUP;
      {Traps custom PJM_LMOUSEUP message, which calls MouseUp event with given
      info. Can be used to simulate mouse up events on and off the button to
      release button even when mouse is captured by, and released by any
      attached master button. Only works when control's mouse events are
      captured by master button}
    procedure PJMLMouseMove(var Msg: TMessage); message PJM_LMOUSEMOVE;
      {Traps custom PJM_LMOUSEMOVE message, which calls MouseMove event with
      given info. Can be used to simulate mouse move events on and off the
      button to keep button down when mouse over master button. Only works when
      control's mouse events are captured by master button}
    procedure PJMCaptureControl(var Msg: TMessage); message PJM_CAPTURECONTROL;
      {Traps custom PJM_CAPTURECONTROL message. This message is sent by any
      attached master button to permit false mouse messages to be sent to
      control state of button and also to inhibit this button from firing mouse
      events}
    procedure PJMAttach(var Msg: TMessage); message PJM_ATTACH;
      {Traps custom PJM_ATTACH message. This message is sent by a master button
      when it wishes to attach to or detach from the menu button. The WParam
      field of the message refers to the master button object. Uses the Attach
      method to perform the actual attachment}
    { New methods }
    procedure Attach(AButton: TPJLinkedSpeedButton); virtual;
      {Attaches this button as slave button of given master button. If AButton
      is nil then this button is detached from any current master button}
  public
    { Overrides }
    constructor Create(AOwner: TComponent); override;
      {Class constructor}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      {Override that only permits changes in width when button is associated
      with a master button}
    { Read only property }
    property MasterButton: TPJLinkedSpeedButton read fMasterButton;
      {Read only property that provides reference to this control's master
      button}
  published
    { Redefined properties }
    property Flat: Boolean
      read GetFlat write SetFlat;
    property Glyph: TBitmap
      read GetGlyph write SetGlyph stored StoreGlyph;
    { Newly published inherited properties }
    property ActiveMenu;
    property ReleaseButton;
    { New property }
    property UseDefaultGlyph: Boolean
      read fUseDefaultGlyph write SetUseDefaultGlyph default True;
  end;


  {
  TPJLinkedSpeedButton:
    A speed button that can work in association with a linked slave popup menu
    speed button. When this button is activated or clicked it captures the slave
    button and makes it act as if part of this button.

    Inheritance: TPJLinkedSpeedButton => TPJUngroupedSpeedButton
      => [TSpeedButton]
  }
  TPJLinkedSpeedButton = class(TPJUngroupedSpeedButton)
  private // properties
    fMenuButton: TPJLinkedMenuSpeedButton;
    procedure SetMenuButton(const Value: TPJLinkedMenuSpeedButton);
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
  private // other
    procedure CaptureMenuBtn(Flag: Boolean);
      {"Capture" or releases the mouse events of the menu button according to
      state of flag: when true this prevents mouse events in menu button from
      firing or re-enables them when false}
    procedure MouseMsgToMenuBtn(Msg: Word; Shift: TShiftState; X, Y: Integer);
      {Passes the a mouse message or type Msg, passing given shift state and
      mouse coordinates to menu button}
    procedure AttachMenuBtn(Btn: TPJLinkedMenuSpeedButton);
      {Send PJM_ATTACH message to given menu button: causes the given button
      to attach itself to this button}
    procedure DetachMenuBtn(Btn: TPJLinkedMenuSpeedButton);
      {Send PJM_ATTACH message to given menu button: causes the given button
      to detach itself to this button}
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {Override of Mouse down method. This method causes button to be pressed.
    When this happens we also want any associated menu button to be depressed,
    but not to fire any of its own mouse events}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
      {Override of mouse up method. If the mouse up message is received when
      mouse is over speed button the button is restored and the OnClick event is
      fired. If mouse is released over any attached menu button, we still want
      this processing to occur - and our button's OnClick tevent to fire. We
      also wish to release the attached menu button, which will have been
      depressed by MouseDown method}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      {Override of mouse move method. Normally if mouse tracks off the button it
      is restored to normal state without firing an OnClick event. Where we have
      an attached menu button we need to adapt processing as follows:
      + Mouse is moved over this button: do default processing
      + Mouse is moved over menu button: act as if over this button
      + Mouse is moved off both buttons: restore both buttons
      + Mouse is dragged back over these buttons after moving off: depress both
        buttons again}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
      {Override of notification method to set menubutton to nil when it is
      destroyed}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      {CM_MOUSEENTER message handler: acts on event and passes on to any
      attached menu button. This handler is required to switch on highlighting
      on two associated buttons at same time}
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      {CM_MOUSELEAVE message handler: acts on event and passes on to any
      attached menu button. This handler is required to switch on highlighting
      on two associated buttons at same time}
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {CM_ENABLEDCHANGED message handler: sets Enabled property of any attached
      menu button to be same as this button's state}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      {Override that adjusts position and size of any associated menu button to
      match and stay attached to this button}
  published
    { Redefined property }
    property Flat: Boolean
      read GetFlat write SetFlat;
    { New property }
    property MenuButton: TPJLinkedMenuSpeedButton
      read fMenuButton write SetMenuButton;
      {Reference to the associated menu speed button}
  end;


procedure Register;
  {Delphi registration routine}


implementation


{$R PJMenuSpeedButtons.res}


procedure Register;
  {Registers the components with Delphi}
begin
  RegisterComponents(
    'DelphiDabbler',
    [TPJMenuSpeedButton, TPJLinkedSpeedButton, TPJLinkedMenuSpeedButton]
  );
end;


{ TPJUngroupedSpeedButton }

function TPJUngroupedSpeedButton.GetGroupIndex: Integer;
  {Read access method for reimplemented GroupIndex property: always returns 0}
begin
  Result := 0;
end;

procedure TPJUngroupedSpeedButton.SetGroupIndex(const Value: Integer);
  {Write access methed for reimplemented GroupIndex property: discards given
  value}
begin
  {Do nothing}
end;


{ TPJCustomMenuSpeedButton }

procedure TPJCustomMenuSpeedButton.Click;
  {Override of method that triggers OnClick event and / or Action's OnExecute
  event. The events may be inhibited in some circumstances to ensure that they
  are fired immediately before menu appears}
begin
  if not fInhibitClick then
    inherited Click;
end;

constructor TPJCustomMenuSpeedButton.Create(AOwner: TComponent);
  {Class constructor: sets default values}
begin
  inherited Create(AOwner);
  fInhibitClick := False;
  fReleaseButton := False;
  fActiveMenu := nil;
  fMenuPlacing := mpBottomLeft;
end;

procedure TPJCustomMenuSpeedButton.DisplayMenu(Pos: TPoint);
  {Pops up the atached menu}
begin
  if Assigned(fActiveMenu) then
    fActiveMenu.Popup(Pos.X, Pos.Y);
end;

procedure TPJCustomMenuSpeedButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Method called when user releases mouse button after clicking speed button:
  constructs and then pops-up colour menu. Also ensures that OnClick event is
  fired just before menu appears}
begin
  // Stop activation of any OnClick event triggered by inherited MouseUp method
  fInhibitClick := True;
  // Check if mouse up is over button: only display menu if so
  if (X >= 0) and (X < ClientWidth)
    and (Y >= 0) and (Y <= ClientHeight) then
  begin
    // Mouse cursor is over button
    // trigger an OnClick event before menu appears
    if Button = mbLeft then
      inherited Click;
    if fReleaseButton then
      // restore button to up state before menu appears
      inherited MouseUp(Button, Shift, X, Y);
    if (Button = mbLeft) and not (csDesigning in ComponentState) then
      // display the menu
      DisplayMenu(ClientToScreen(PopupPosition));
    if not fReleaseButton then
      // restore button to up state after menu is closed
      inherited;
    // allow activation of any Click methods
    fInhibitClick := False;
  end
  else
    // Mouse button not over button: no menu, just restore button state
    inherited;
end;

procedure TPJCustomMenuSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Resets reference to ActiveMenu to nil if the associated popup menu component
  is being freed}
begin
  if (AComponent = fActiveMenu) and (Operation = opRemove) then
    fActiveMenu := nil;
end;

function TPJCustomMenuSpeedButton.PopupPosition: TPoint;
  {Returns the point, in screen coordinates, where the popup menu is to be
  displayed}
begin
  case fMenuPlacing of
    mpTopLeft:      Result := Point(0, 0);
    mpTopRight:     Result := Point(Width, 0);
    mpBottomLeft:   Result := Point(0, Height);
    mpBottomRight:  Result := Point(Width, Height);
  end;
end;

procedure TPJCustomMenuSpeedButton.SetActiveMenu(const Value: TPopupMenu);
  {Write access method for ActiveMenu property: records new menu value and
  informs new menu that this object needs to be informed if the menu is freed}
begin
  if Value <> fActiveMenu then
  begin
    fActiveMenu := Value;
    if Assigned(fActiveMenu) then
      fActiveMenu.FreeNotification(Self);
  end;
end;


{ TPJLinkedMenuSpeedButton }

const
  {Type of current glyph: either default glyph or user provided (or nil) glyph}
  GS_DEFAULT = 1;
  GS_USER = 2;

procedure TPJLinkedMenuSpeedButton.Attach(AButton: TPJLinkedSpeedButton);
  {Attaches this button as slave button of given master button. If AButton is
  nil then this button is detached from any current master button}
begin
  // Check if value has changed
  if fMasterButton <> AButton then
  begin
    // Detach self from any existing master button
    if Assigned(fMasterButton) then
      fMasterButton.MenuButton := nil;
    // Record new master button (if any)
    fMasterButton := AButton;
    if Assigned(AButton) then
    begin
      // Move self to same parent control as master button if necessary
      if Parent <> AButton.Parent then
        Parent := AButton.Parent;
      // Align to new master button's RHS
      SetBounds(0, 0, Width, 0);  // SetBounds uses fMasterButton's size and pos
      // Make own flat and enabled properties same as master
      Flat := AButton.Flat;
      Enabled := AButton.Enabled;
    end;
  end;
end;

procedure TPJLinkedMenuSpeedButton.CMEnabledChanged(var Msg: TMessage);
  {CM_ENABLEDCHANGED message handler: if any master button's enabled
  property is false then this button's state must remain false}
begin
  inherited;
  if Assigned(fMasterButton) and not fMasterButton.Enabled then
    Enabled := False;
end;

procedure TPJLinkedMenuSpeedButton.CMMouseEnter(var Msg: TMessage);
begin
  {CM_MOUSEENTER mesasge handler: acts on event and passes on to any attached
  master button. Message passed on is specially cutomised to prevent infinite
  recursion. This handler is required to switch on highlighting on two
  associated buttons at same time}
  inherited;
  if MouseInControl and Assigned(fMasterButton) then
    fMasterButton.Perform(CM_MOUSEENTER, 1, 0);
end;

procedure TPJLinkedMenuSpeedButton.CMMouseLeave(var Msg: TMessage);
  {CM_MOUSELEAVE mesasge handler: acts on event and passes on to any attached
  master button. Message passed on is specially cutomised to prevent infinite
  recursion. This handler is required to switch off highlighting on two
  associated buttons at same time}
begin
  inherited;
  if Assigned(fMasterButton) then
    fMasterButton.Perform(CM_MOUSELEAVE, 1, 0);
end;

constructor TPJLinkedMenuSpeedButton.Create(AOwner: TComponent);
  {Class constructor: sets default values and required event handler}
begin
  inherited Create(AOwner);
  // Set change event handler for Glyph
  Glyph.OnChange := GlyphChange;
  // Set default values
  Width := 14;
  fMasterButton := nil;
  fControlCaptured := False;
  SetUseDefaultGlyph(True);         // creates and sets initial default glyph
end;

procedure TPJLinkedMenuSpeedButton.DisplayMenu(Pos: TPoint);
  {Inhibits display of menu by MouseUp method when control is captured}
begin
  if not fControlCaptured then
    inherited;
end;

procedure TPJLinkedMenuSpeedButton.EnableMouseEvents(Flag: Boolean);
  {Disables or restores this buttons mouse event handler according to flag}
begin
  if Flag then
  begin
    // Restore stored event handlers
    OnMouseMove := fMouseMoveEventStore;
    OnMouseDown := fMouseDownEventStore;
    OnMouseUp := fMouseUpEventStore;
    OnClick := fMouseClickEventStore;
    OnDblClick := fMouseDblClickEventStore;
  end
  else
  begin
    // Save and disable event handlers
    fMouseMoveEventStore := OnMouseMove;
    fMouseDownEventStore := OnMouseDown;
    fMouseUpEventStore := OnMouseUp;
    fMouseClickEventStore := OnClick;
    fMouseDblClickEventStore := OnDblClick;
    OnMouseMove := nil;
    OnMouseDown := nil;
    OnMouseUp := nil;
    OnClick := nil;
    OnDblClick := nil;
  end;
end;

function TPJLinkedMenuSpeedButton.GetFlat: Boolean;
  {Replacement read access method for Flat property: simply calls inherited
  method}
begin
  Result := inherited Flat;
end;

function TPJLinkedMenuSpeedButton.GetGlyph: TBitmap;
  {Replacement read access method for Glyph property: simply calls inherited
  method}
begin
  Result := inherited Glyph;
end;

procedure TPJLinkedMenuSpeedButton.GlyphChange(Sender: TObject);
  {Handles event triggered when Glyph property changes: assumes that the glyph
  that has been set is user provided. If this is not the case then other code
  changes this value}
begin
  fGlyphStyle := GS_USER;
end;

procedure TPJLinkedMenuSpeedButton.LoadDefGlyph;
  {Loads the default glyph from resources into the Glyph property}
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(HInstance, 'DROPDOWNARROW');
    inherited Glyph := Bmp;
  finally
    Bmp.Free;
  end;
end;

procedure TPJLinkedMenuSpeedButton.Loaded;
  {Override of loaded method to check if we must destroy the initial default
  glyph: in cases where no glyph is required}
begin
  inherited Loaded;
  if not fUseDefaultGlyph and (fGlyphStyle = GS_DEFAULT) then
    inherited Glyph := nil;
end;

procedure TPJLinkedMenuSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Override of notification method to set master button to nil when it is
  destroyed}
begin
  if (AComponent = fMasterButton) and (Operation = opRemove) then
    fMasterButton := nil;
end;

procedure TPJLinkedMenuSpeedButton.PJMAttach(var Msg: TMessage);
  {Traps custom PJM_ATTACH message. This message is sent by a master button when
  it wishes to attach to or detach from the menu button. The WParam field of the
  message refers to the master button object. Uses the Attach method to perform
  the actual attachment}
var
  Obj: TObject; // the master speed button object
begin
  Obj := TObject(Msg.WParam);
  Attach(Obj as TPJLinkedSpeedButton);
end;

procedure TPJLinkedMenuSpeedButton.PJMCaptureControl(var Msg: TMessage);
  {Traps custom PJM_CAPTURECONTROL message. This message is sent by any attached
  master button to permit false mouse messages to be sent to control state of
  button and also to inhibit this button from firing mouse events}
begin
  SetControlCaptured(Msg.WParam = 1);
end;

procedure TPJLinkedMenuSpeedButton.PJMLMouseDown(var Msg: TMessage);
  {Traps custom PJM_LMOUSEDOWN message, which calls MouseDown event with given
  info. Can be used to simulate mouse down events on and off the button to force
  button down, even when mouse is over any attached mastrer button. Only works
  when control's mouse events are captured by master button}
var
  MouseMsg: TWMMouse;
begin
  if fControlCaptured then
  begin
    MouseMsg := TWMMouse(Msg);
    MouseDown(mbLeft, KeysToShiftState(MouseMsg.Keys),
      MouseMsg.XPos, MouseMsg.YPos);
  end;
end;

procedure TPJLinkedMenuSpeedButton.PJMLMouseMove(var Msg: TMessage);
  {Traps custom PJM_LMOUSEMOVE message, which calls MouseMove event with given
  info. Can be used to simulate mouse move events on and off the button to keep
  button down when mouse over master button. Only works when control's mouse
  events are captured by master button}
var
  MouseMsg: TWMMouse;
begin
  if fControlCaptured then
  begin
    MouseMsg := TWMMouse(Msg);
    MouseMove(KeysToShiftState(MouseMsg.Keys),
      MouseMsg.XPos, MouseMsg.YPos);
  end;
end;

procedure TPJLinkedMenuSpeedButton.PJMLMouseUp(var Msg: TMessage);
  {Traps custom PJM_LMOUSEUP message, which calls MouseUp event with given info.
  Can be used to simulate mouse up events on and off the button to release
  button even when mouse is captured by, and released by any attached master
  button. Only works when control's mouse events are captured by master button}
var
  MouseMsg: TWMMouse;
begin
  if fControlCaptured then
  begin
    MouseMsg := TWMMouse(Msg);
    MouseUp(mbLeft, KeysToShiftState(MouseMsg.Keys),
      MouseMsg.XPos, MouseMsg.YPos);
  end;
end;

procedure TPJLinkedMenuSpeedButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
  {Override that only permits changes in width when button is associated with
  a master button}
begin
  if Assigned(fMasterButton) then
    inherited SetBounds(fMasterButton.Left + fMasterButton.Width,
      fMasterButton.Top, AWidth, fMasterButton.Height)
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TPJLinkedMenuSpeedButton.SetControlCaptured(Flag: Boolean);
  {Sets flag that shows if control is captured (ie mouse events are taken over
  by any associated master button). Disable/enables mouse events depending on
  whether captured}
begin
  if Flag <> fControlCaptured then
  begin
    fControlCaptured := Flag;
    EnableMouseEvents(not fControlCaptured);
  end;
end;

procedure TPJLinkedMenuSpeedButton.SetFlat(const Value: Boolean);
  {Replacement write access method for Flat property: if there's an associated
  master button, the value is only changed if new value matches that of master
  button}
begin
  // This method never called when streaming in
  if Assigned(fMasterButton) then
  begin
    if Value = fMasterButton.Flat then
      inherited Flat := Value;
  end
  else
    inherited Flat := Value;
end;

procedure TPJLinkedMenuSpeedButton.SetGlyph(const Value: TBitmap);
  {Replacement write access method for Glyph property: sets Glyph and records
  that we're not using default glyph}
begin
  // This method never called when streaming in
  inherited Glyph := Value;
  fUseDefaultGlyph := False;
end;

procedure TPJLinkedMenuSpeedButton.SetUseDefaultGlyph(
  const Value: Boolean);
  {Write access method for UseDefaultGlyph property: if value is true then the
  default glyph is loaded into the Glyph property and if false any existing
  Glyph is deleted. On loading component, if value is true (default) it won't be
  in form file, so default glyph created in contructor is used (there is no
  Glyph in form file to overwrite it). If value is false on loading there could
  be a Glyph in form file that would be deleted by this method since this
  property will be stored in form file and property is loaded after Glyph, so we
  defer any deletion to Loaded method when we know if there's been a Glyph
  loaded}
begin
  if fUseDefaultGlyph <> Value then
  begin
    // There's something to do
    if Value then
    begin
      // Load the glyph if we're not Loading and record that we have default
      if not (csLoading in ComponentState) then
        LoadDefGlyph;                  // changes Glyph => fGlyphStyle = GS_USER
      fGlyphStyle := GS_DEFAULT;
    end
    else
    begin
      // Delete the glyph: don't do this at design: defer till Loaded
      if not (csLoading in ComponentState) then
        inherited Glyph := nil;        // changes Glyph => fGlyphStyle = GS_USER
    end;
    // Record the property value
    fUseDefaultGlyph := Value;
  end;
end;

function TPJLinkedMenuSpeedButton.StoreGlyph: Boolean;
  {Returns true if the Glyph property should be streamed to form resource by
  designer. Glyph is only stored if it's not the DefaultGlyph}
begin
  Result := not fUseDefaultGlyph;
end;


{ TPJLinkedSpeedButton }

procedure TPJLinkedSpeedButton.AttachMenuBtn(
  Btn: TPJLinkedMenuSpeedButton);
  {Send PJM_ATTACH message to given menu button: causes the given button to
  attach itself to this button}
begin
  if Assigned(Btn) then
    Btn.Perform(PJM_ATTACH, Integer(Self), 0);
end;

procedure TPJLinkedSpeedButton.CaptureMenuBtn(Flag: Boolean);
  {"Capture" or releases the mouse events of the menu button according to state
  of flag: when true this prevents mouse events in menu button from firing or
  re-enables them when false}
begin
  if Flag then
    fMenuButton.Perform(PJM_CAPTURECONTROL, 1, 0)
  else
    fMenuButton.Perform(PJM_CAPTURECONTROL, 0, 0);
end;

procedure TPJLinkedSpeedButton.CMEnabledChanged(var Msg: TMessage);
  {CM_ENABLEDCHANGED message handler: sets Enabled property of any attached menu
  button to be same as this button's state}
begin
  inherited;
  if Assigned(fMenuButton) then
    fMenuButton.Enabled := Enabled;
end;

procedure TPJLinkedSpeedButton.CMMouseEnter(var Msg: TMessage);
  {CM_MOUSEENTER message handler: acts on event and passes on to any attached
  menu button. This handler is required to switch on highlighting on two
  associated buttons at same time}
begin
  inherited;
  if MouseInControl and Assigned(fMenuButton) and (Msg.WParam <> 1) then
    fMenuButton.Perform(CM_MOUSEENTER, 0, 0);
end;

procedure TPJLinkedSpeedButton.CMMouseLeave(var Msg: TMessage);
  {CM_MOUSELEAVE message handler: acts on event and passes on to any attached
  menu button. This handler is required to switch on highlighting on two
  associated buttons at same time}
begin
  inherited;
  if Assigned(fMenuButton) and (Msg.WParam <> 1) then
    fMenuButton.Perform(CM_MOUSELEAVE, 0, 0);
end;

constructor TPJLinkedSpeedButton.Create(AOwner: TComponent);
  {Class constructor}
begin
  inherited Create(AOwner);
  fMenuButton := nil;
end;

procedure TPJLinkedSpeedButton.DetachMenuBtn(
  Btn: TPJLinkedMenuSpeedButton);
  {Send PJM_ATTACH message to given menu button: causes the given button to
  detach itself to this button}
begin
  if Assigned(Btn) then
    Btn.Perform(PJM_ATTACH, Integer(nil), 0);
end;

function TPJLinkedSpeedButton.GetFlat: Boolean;
  {Replacement read access method for Flat property: simply calls inherited
  method}
begin
  Result := inherited Flat;
end;

procedure TPJLinkedSpeedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Override of Mouse down method. This method causes button to be pressed. When
  this happens we also want any associated menu button to be depressed, but not
  to fire any of its own mouse events}
begin
  inherited;
  if Assigned(fMenuButton) then
  begin
    // Prevent menu button from firing mouse events
    CaptureMenuBtn(True);
    // Send false mouse down message to depress menu button
    MouseMsgToMenuBtn(PJM_LMOUSEDOWN, Shift, 0, 0);
  end;
end;

procedure TPJLinkedSpeedButton.MouseMove(Shift: TShiftState; X,
  Y: Integer);
  {Override of mouse move method. Normally if mouse tracks off the button it is
  restored to normal state without firing an OnClick event. Where we have an
  attached menu button we need to adapt processing as follows:
  + Mouse is moved over this button: do default processing
  + Mouse is moved over menu button: act as if over this button
  + Mouse is moved off both buttons: restore both buttons
  + Mouse is dragged back over these buttons after moving off: depress both
    buttons again}
var
  InCombinedBtns: Boolean;  // flag true if mouse over this or menu button
begin
  if Assigned(fMenuButton) then
  begin
    // Set flag if mouse over either this control or menu button
    InCombinedBtns := (X >= 0) and (X < ClientWidth + fMenuButton.Width)
      and (Y >= 0) and (Y <= ClientHeight);
    if InCombinedBtns then
    begin
      // We're over one of the buttons
      // do default processing
      inherited MouseMove(Shift, X, Y);
      if X >= ClientWidth then
      begin
        // we're off this button and over menu button
        if fState <> bsDown then
        begin
          // ensure this button stays down
          fState := bsDown;
          Invalidate;
        end;
      end;
      if (fState = bsDown) and not fMenuButton.Down then
      begin
        // this button is down and menu button is up: force it down
        // .. make sure menu button isn't firing mouse events and
        CaptureMenuBtn(True);
        // .. use false mouse down event to force it down
        MouseMsgToMenuBtn(PJM_LMOUSEDOWN, Shift, 0, 0);
      end;
    end
    else
    begin
      // We're off both buttons:
      // do default processing which restores this button
      inherited MouseMove(Shift, X, Y);
      // send false mouse move message to menu button to make it think mouse has
      // moved off it so it restores itself
      MouseMsgToMenuBtn(PJM_LMOUSEMOVE, Shift, -1, -1);
    end;
  end
  else
    inherited;
end;

procedure TPJLinkedSpeedButton.MouseMsgToMenuBtn(Msg: Word; Shift: TShiftState;
  X, Y: Integer);
  {Passes the a mouse message or type Msg, passing given shift state and mouse
  coordinates to menu button}
var
  MenuMsg: TWMMouse;  // the message reocrd
begin
  // Set shift state
  MenuMsg.Keys := 0;
  if ssShift in Shift then MenuMsg.Keys := MenuMsg.Keys or MK_SHIFT;
  if ssCtrl in Shift then MenuMsg.Keys := MenuMsg.Keys or MK_CONTROL;
  // Record mouse position
  MenuMsg.XPos := X;
  MenuMsg.YPos := Y;
  // Send message to menu button
  fMenuButton.Perform(Msg, TMessage(MenuMsg).WParam, TMessage(MenuMsg).LParam);
end;

procedure TPJLinkedSpeedButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Override of mouse up method. If the mouse up message is received when mouse
  is over speed button the button is restored and the OnClick event is fired. If
  mouse is released over any attached menu button, we still want this processing
  to occur - and our button's OnClick tevent to fire. We also wish to release
  the attached menu button, which will have been depressed by MouseDown method}
var
  InCombinedBtns: Boolean;  // flag true if mouse is over this or menu button
begin
  if Assigned(fMenuButton) then
  begin
    // Calc if mouse is over either this button or attached menu button
    InCombinedBtns := (X >= 0) and (X < ClientWidth + fMenuButton.Width)
      and (Y >= 0) and (Y <= ClientHeight);
    // Send a false mouse up message to menu button to cause it to be released
    // menu button mouse events inhibited in MouseDown => its events dont fire
    MouseMsgToMenuBtn(PJM_LMOUSEUP, Shift, 0, 0);
    // can now allow menu button to fire own events again
    CaptureMenuBtn(False);
    // Do usual processing on this button (OnClick event and restore button)
    inherited MouseUp(Button, Shift, X, Y);
    // If mouse was released over menu button do inherited MouseUp won't have
    // redrawn button or triggered OnClick event, so do it here
    if InCombinedBtns and (X >= ClientWidth) then
    begin
      if not (fState in [bsExclusive, bsDown]) then
        Invalidate;
      Click;
    end;
  end
  else
    // We have no attached menu button so do default processing
    inherited;
end;

procedure TPJLinkedSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Override of notification method to set menubutton to nil when it is
  destroyed}
begin
  if (AComponent = fMenuButton) and (Operation = opRemove) then
    fMenuButton := nil;
end;

procedure TPJLinkedSpeedButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
  {Override that adjusts position and size of any associated menu button to
  match and stay attached to this button}
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Assigned(fMenuButton) then
    // menu buttons are always aligned to our top and right side, and are always
    // same height as this button
    fMenuButton.SetBounds(ALeft + AWidth, ATop, fMenuButton.Width, ATop);
end;

procedure TPJLinkedSpeedButton.SetFlat(const Value: Boolean);
  {Replacement write access method for Flat property: if there's an associated
  menu button, its Flat property is changed in tandem with this one}
begin
  inherited Flat := Value;
  if Assigned(fMenuButton) then
    fMenuButton.Flat := Value;
end;

procedure TPJLinkedSpeedButton.SetMenuButton(
  const Value: TPJLinkedMenuSpeedButton);
  {Write access method for MenuButton property: records new value, unlinks any
  existing menu button and attaches new one}
var
  OldMB: TPJLinkedMenuSpeedButton;  // reference to any existing menu button
begin
  if fMenuButton <> Value then
  begin
    // Detach any existing menu button
    OldMB := fMenuButton;
    if Assigned(OldMB) then
    begin
      fMenuButton := nil;     // prevents recursion
      DetachMenuBtn(OldMB);
    end;
    // Record new value
    fMenuButton := Value;
    // Associate new menu button with this one
    if Assigned(Value) then
    begin
      Value.FreeNotification(Self);
      AttachMenuBtn(Value);
      if Assigned(OldMB) then
        // nudge previous menu button out of way so still visible
        OldMB.Left := Value.Left + Value.Width;
    end;
  end;
end;

end.
