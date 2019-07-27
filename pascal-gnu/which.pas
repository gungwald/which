program Which;

uses
    gpc;

type
    Vector = array[0..255] of String(128);

var
    programToFind: String(1024);
    path: String(1024);
    directory: String;
    pathExt: String(1024);
    extensions: Vector;
    extensionCount: Integer;
    i: Integer;
    someFile: String(1024);

(**
 * Destructively parses beingTokenized.
 *)
function nextToken(var beingTokenized: String;
                   separator: String;
                   var token: String): Boolean;
var
    location: Integer;
    hasNextToken: Boolean;
begin
    location := index(beingTokenized, separator);
    if location > 0 then
      begin
        hasNextToken := True;
        token := subStr(beingTokenized, 1, location - 1);
        beingTokenized := subStr(beingTokenized, location + length(separator));
      end 
    else if length(beingTokenized) > 0 then
      begin
        hasNextToken := true;
        token := beingTokenized;
        beingTokenized := '';
      end 
    else
      begin
        hasNextToken := false;
      end;
    nextToken := hasNextToken;  (* Return whether there is another token *)
end;

(**
 * Split a String into parts and put the parts into a Vector.
 *)
function split(splittethMe: String; thySplittor: Char; var splitten: Vector): Integer;
var
    count: Integer;
    part: String(10);
begin
    count := 0;
    while nextToken(splittethMe, ';', part) do begin
        splitten[count] := part;
        count := count + 1;
    end;
    split := count;  (* Return how many parts it was split into *)
end;

procedure correctFileNameCase(var absFileName: String);
var
    parentDirectoryName: String(1024);
    parentDirectory: DirPtr;
    shortFileName: String(256);
    someFile: String(256);
    done: Boolean;
begin
    shortFileName := nameFromPath(absFileName) + extFromPath(absFileName);
    parentDirectoryName := dirFromPath(absFileName);
    parentDirectory := openDir(parentDirectoryName);
    done := false;
    while not done do begin
        someFile := readDir(parentDirectory);
        if strEqualCase(someFile, shortFileName) then begin
            done := true;
            absFileName := parentDirectoryName + someFile;
        end;
    end;
end;

function isExecutable(var name: String): Boolean;
var
    fileToTest: Bindable File;
    binder: BindingType;
    executable: Boolean;
begin
    executable := false;
    binder.name := name;
    bind(fileToTest, binder);
    binder := binding(fileToTest);
    if binder.existing then begin
        executable := true;
        correctFileNameCase(name);
    end;
    unbind(fileToTest);
    isExecutable := executable;
end;

(* Main Program *)
begin
    if paramCount <> 1 then begin
        writeln('usage: which <program_name>');
        halt(0);
    end;
    programToFind := paramStr(1);

    pathExt := getenv('PATHEXT');
    extensionCount := split(pathExt, ';', extensions);

    path := getenv('PATH');
    while nextToken(path, ';', directory) do begin
        for i := 0 to extensionCount - 1 do begin
            someFile := directory + '\' + programToFind + extensions[i];
            if isExecutable(someFile) then begin
                writeln(someFile);
            end;
        end;
    end;
end.

