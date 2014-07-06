# BANCNova Assembly Syntax

BANCNova uses an assembly syntax to represent programs.  The general syntax looks like this:

 `STARTCOND @42 == 'Y'`
 `SET @363, @363, ADD(20)`
 `SHOW @363, 1904, 15, 1911`
 `ENDCOND`
 `COND @42 == 'N'`
 `GOTO 1`

The initial word describes the instruction.  The `STARTCOND`/`ENDCOND` are a structural conditional statement; they assemble into a similar pair of BANCStar instructions.  `COND` is a conditional which only encloses the next line.

The `@363` syntax is used to denote BANCStar "prompts", which seem to be like variable cells with a few bits of metadata we are trying to understand.  `@363` represents prompt cell 363.

The `SET` command above is setting `@363` to its current value plus 20.

The `SHOW` command above is displaying prompt 363's label at <4, 19>, and a 15-character wide input field at <11, 19>; the latter is filled with the prompt's value.



## Conditionals
Conditional instructions share some syntax.  They all use the following general form:

 `COND *<prompt>* *<cmp>* *<value>*`

`*<cmp>*` currently only represents the numeric operators, which can result in misleading assembly/disassembly if the `*<prompt>*` is of string type (see the list of bugs at the end of this document).

Valid numeric comparison operators are `<`, `>`, `<=`, `>=`, `==`, and `!=`.

Two unary conditions are also available:

 `COND ISNULL(*<prompt>*)`
 `COND ISNOTNULL(*<prompt>*)`

### `COND`

Simple conditional instruction.

 `COND *<condition>*`
 `STATEMENT`

The `STATEMENT` above will only be executed if the condition is true.

### `RCOND`

Simple reverse-conditional instruction.

 `COND *<condition>*`
 `STATEMENT`

The `STATEMENT` above will only be executed if the condition is false.

### `STARTCOND`/`ENDCOND`

Block conditional instruction.

 `STARTCOND *<condition>*`
 ...
 `ENDCOND`

The statements from the `STARTCOND` until the *next* `ENDCOND` will be executed only if the condition is true.

Note that BANCStar does *not* seem to nest `STARTCOND`/`ENDCOND` pairs: it simply scans linearly from the `STARTCOND` until it finds the next `ENDCOND`, and terminates there.

### `STARTRCOND`/`ENDRCOND`

Block reverse-conditional instruction.

 `STARTRCOND *<condition>*`
 ...
 `ENDCOND`

As for `STARTCOND`, but only if the condition is false.



## Other flow-control instructions
### `GOTO`

 `GOTO *<page number>*`
 `GOTO *<prompt>*`

The `*<page number>*` is a numeric literal.  The exact location of a "page boundary" where control transfers to appears to be dependent on the meaning of the `NEWPAGE` instruction (see below).

The `*<prompt>*` form is believed to read a page number from the contents of the cell in question.

### `GOTOF`

Go to F-key.

 `GOTOF *<number [1, 20]>*`

This instruction assembles to BANCStar instructions `501` through `520`.  It's not clear what it means to "go" to a particular F-key.  Possibly the function keys are wired to particular subroutines in some fashion.

### `GOTOTR`

Go to transaction.

 `GOTOTR *<number [1, 24]>*`

This instruction assembles to BANCSTAR isntructions `4001` through `4024`.  It's a complete mystery what a "transaction" is and how you would "go" to one.

### `GOSTORAGE`

Go to "Storage".

 `GOSTORAGE`

BANCSTAR instruction `4083`.  A mystery.

### `GOPRODUCT`

Go to "Product & Sales".

 `GOPRODUCT`

BANCSTAR instruction `4080`.  A mystery.

### `EXIT`

Exit BANCStar.

 `EXIT`

Presumably halts the BANCStar virtual machine.

### `MAINMENU`

Go to main menu.

 `MAINMENU`

Presumably transfers control to `MM1SM1`.  We don't know if it enters at the top of the file or not.

### `MTASKMENU`

Go to multi-task menu.

 `MTASKMENU`

BANCStar instruction `4084`.  We don't know what the "multi-task menu" is, but I would bet that it's not any form of concurrency.



## Arithmetic/variable manipulation instructions

The first argument `*<dest>*` to each of the below commands is the prompt cell into which the result of the computation should be placed.

Many of the below commands use "arithterms", which are arguments with the syntax `ADD(@42)`.  For example:

 `SET @729, 12, MUL(@169), ADD(56)`

This means to set prompt cell `@729`'s value to: `12`, times `@169`, plus `56`.

Valid arithterm names are `ADD`, `SUB`, `MUL`, and `DIV`.

It's not known how division-by-zero, overflow, and other such special cases are handled.

### `CLEAR`

Clears a cell.

 `CLEAR *<dest>*, *<prompt>*`

In samples, `*<prompt>*` always seems to be equal to `*<dest>*`.  This might be a requirement.

It's not known if this is distinct from `SET`ting the cell to zero or not.  May depend on cell type.

Probably equivalent in function to `SUBSTR *<dest>*, *<prompt>*, 0, 0`, but assembles to a slightly different BANCStar line.

### `LEN`

Get length.

 `LEN *<dest>*, *<value/prompt>*`
 `LEN *<dest>*, *<value/prompt>*, *<arithterm>*`
 `LEN *<dest>*, *<value/prompt>*, *<arithterm>*, *<arithterm>*`

`*<value/prompt>*` can be either a cell (in which case take the length of its contents, presumably a string), or a literal number (in which case it's not clear what is being measured).

Optionally, up to two extra operations can be done afterwards, by specifying trailing `*<arithterm>*`s.

### `POW`

Exponentiation.

 `POW *<dest>*, *<x>*, *<y>*`
 `POW *<dest>*, *<x>*, *<y>*, *<arithterm>*`

Sets `*<dest>*` to the value of `*<x>*` raised to the power of `*<y>*`.

Optionally, an extra operation can be done afterwards, by specifying a trailing `*<arithterm>*`.

### `INVPOW`

Exponentiation to negative powers.

 `INVPOW *<dest>*, *<x>*, *<y>*`
 `INVPOW *<dest>*, *<x>*, *<y>*, *<arithterm>*`

Sets `*<dest>*` to the value of `*<x>*` raised to the power of negative-`*<y>*`.

Note that the resulting BANCStar code is speculative; it's not known if the results will be accurate.

Optionally, an extra operation can be done afterwards, by specifying a trailing `*<arithterm>*`.

### `ROOT`

Find the nth root of a value.

 `ROOT *<dest>*, *<x>*, *<y>*`
 `ROOT *<dest>*, *<x>*, *<y>*, *<arithterm>*`

Sets `*<dest>*` to the value of the `*<y>*`-th root of `*<x>*`.

Note that the resulting BANCStar code is speculative; it's not known if the results will be accurate.

Optionally, an extra operation can be done afterwards, by specifying a trailing `*<arithterm>*`.

### `SUBSTR`

Extract substring

 `SUBSTR *<dest>*, *<str>*, *<start>*, *<length>*`

Sets `*<dest>*` to the substring of `*<str>*` from `*<start>*`; the resulting string has `*<length>*` characters.

It's not known whether strings are indexed from 0 or from 1, and it's not known what happens if the substring exceeds the bounds of the input string.

### `TIME`

Get system time.

 `TIME *<dest>*, *<prompt>*`
 `TIME *<dest>*, *<prompt>*, *<arithterm>*`

Sets `*<dest>*` to the value of the current system time.  It's not known what format this is in.

It's not known what `*<prompt>*` is for, but in samples it always seems to be equal to `*<dest>*`.

Optionally, an extra operation can be done afterwards, by specifying a trailing `*<arithterm>*`.

### `LOG`

Logarithm, presumably to base e but this is not known for sure.

 `LOG *<dest>*, *<x>*`
 `LOG *<dest>*, *<x>*, *<arithterm>*`
 `LOG *<dest>*, *<x>*, *<arithterm>*, *<arithterm>*`

Sets `*<dest>*` to the value of log(`*<x>*`).

Optionally, up to two extra operations can be done afterwards, by specifying trailing `*<arithterm>*`s.

### `TRUNC`

Truncate integer.

 `TRUNC *<dest>*, *<x>*`
 `TRUNC *<dest>*, *<x>*, *<arithterm>*`
 `TRUNC *<dest>*, *<x>*, *<arithterm>*, *<arithterm>*`

Apparently this returns "the positive integer portion" of the argument.  It's not understood how this relates to all datatypes.

Optionally, up to two extra operations can be done afterwards, by specifying trailing `*<arithterm>*`s.

### `NEG`

Negate value.

 `NEG *<dest>*, *<value/prompt>*`
 `NEG *<dest>*, *<value/prompt>*, *<arithterm>*`
 `NEG *<dest>*, *<value/prompt>*, *<arithterm>*, *<arithterm>*`

This negates the argument and stores it in `*<dest>*`.

Optionally, up to two extra operations can be done afterwards, by specifying trailing `*<arithterm>*`s.

### `DATE365`

Date routine #1: "Future Date 365"

 `DATE365 *<dest>*, *<x>*, *<y>*`

The date routines are very poorly understood.
It appears that `*<x>*` and `*<y>*` can be either prompts or numbers.

### `DATE360`

Date routine #2: "Future Date 360"

 `DATE360 *<dest>*, *<x>*, *<y>*`

The date routines are very poorly understood.
It appears that `*<x>*` and `*<y>*` can be either prompts or numbers.

### `DATEDIFF`

Date routine #3: Number of days between dates.

 `DATEDIFF *<dest>*, *<x>*, *<y>*`

This date routine is probably the simplest-looking.  It calculates the number of days between two dates.  It's not known how the dates are represented.
It appears that `*<x>*` and `*<y>*` can be either prompts or numbers.

### `DATEFOO`

Date routine #4: "Future Date 360 and number of days"

 `DATE360 *<dest>*, *<x>*, *<y>*`

The date routines are very poorly understood, and this one is understood the least of all.
It appears that `*<x>*` and `*<y>*` can be either prompts or numbers.

### `SET` (arithmetic only)

Simple arithmetic

 `SET *<dest>*, *<value/prompt>*`
 `SET *<dest>*, *<value/prompt>*, *<arithterm>*`
 `SET *<dest>*, *<value/prompt>*, *<arithterm>*, *<arithterm>*`

Calculates the result of the specified terms and stores the result in `*<dest>*`.

For example:

 `SET @169, 3`

sets cell `@169` to `3`.  A more complex example:

 `SET @729, @169, ADD(@42), DIV(2)`

takes the value of cell `@169`, adds the value of cell `@42`, and divides by `2`; the result is stored in cell `@729`.

### `SET` (general form)

General manipulation.

 `SET *<dest>*, *<value/prompt/extarith>*`
 `SET *<dest>*, *<value/prompt/extarith>*, *<extarith>*`
 `SET *<dest>*, *<value/prompt/extarith>*, *<extarith>*, *<extarith>*`

This form of `SET` is more involved and is the general form of arithmetic statement.  Its use is discouraged.

The extended arithterms (`*<extarith>*`) have extra names besides the four mentioned above.  These are `LEN`, `SUBSTR`, `SYSTEM`, `LOG`, `TRUNC`, and `DATE`.

No documentation is currently provided for the general form; suffice to say it is just a fancy `RAW` statement for dealing with arithmetic.

If you find a real-world BANCStar program that disassembles into this form, please file a bug.



## Display instructions

Some commands use "packed positions", where a row/column pair are packed into a single numeric literal.
The packed numbers are of the form `(y*100 + x)`, where `y` is the row number and `x` is the column number.

### `SHOW`

Show prompt on-screen.

 `SHOW *<prompt>*, *<packed position>*, *<length>*, *<packed position>*`

This instruction displays the specified `*<prompt>*`'s label and/or value on the screen.  When BANCStar is running the code, the value usually results in an editable field.

The first `*<packed position>*` specifies the position of the label, the second is the position of the value.  It's possible to give `0` for either, resulting in that component not being displayed.  The `*<length>*` is the length of the input field for the value; again, specifying `0` here will mean the value is not displayed.

It's not known how the length of the input field specified here relates to the length metadata on the prompt cells themselves.

### `VIDEOMODE`

Set video mode.

 `VIDEOMODE *<mode number>*`

It's not known what the numbers represent: possibly they are just VESA modes.  The only attested value seen so far is `3`.

### `WINDOW`

Set window.

 `WINDOW *<colour pair>*, *<packed position>*, *<packed position>*`

It's not clear whether the two positions are opposite corners, or one corner plus a width/height pair.

The colour pair is a packed number of the form `(bg*16 + fg)`, where `fg` and `bg` are ANSI colour codes for the foreground/background colours.  The high 8 ANSI colours are interpreted as blinking rather than bright.



## Table commands

It's not yet really understood what a "table" is.  It is related to prompts, possibly a structured value can be stored in a prompt??

Each table command must have a `V` or `D` suffix (marked `%` below), denoting whether the "value" or "description" variant is being used.  The meaning of this in BANCStar is unclear.

### `TABLESEARCH%`

Table search.

 `TABLESEARCH% *<prompt>*, *<prompt/value>*, *<x>*, *<y>*`

It's not understood how this works.

### `TABLERANGE%`

Table search range.

 `TABLERANGE% *<prompt>*, *<prompt/value>*, *<x>*, *<y>*`

It's not understood how this works.

### `TABLERANGEP%`

Table search range pairs.

 `TABLERANGEP% *<prompt>*, *<prompt/value>*, *<x>*, *<y>*`

It's not understood how this works.

### `TABLEXFER%`

Table transfer.

 `TABLEXFER% *<prompt>*, *<prompt/value>*, *<x>*, *<y>*`

It's not understood how this works.

### `TABLERXFER%`

Reverse table transfer.

 `TABLERXFER% *<prompt>*, *<prompt/value>*, *<x>*, *<y>*`

It's not understood how this works.



## Data-model commands

Like table commands, there are some suffixes we don't understand.

There is a numeric variant (marked `0` below) whose meaning is a complete mystery right now: it can take the values `0` and `1`.

There is also a suffix (marked `%`) which describes the type variant being used: it take the values `N` ("number"), `L` ("label"), or `V` ("value").

### `DATARUN0%`

Run procedure.

 `DATARUN0%`
 `DATARUN0% *<prompt1>*`
 `DATARUN0% *<prompt1>*, *<prompt2>*`

It's not known exactly what this means.  It appears to be some sort of database.

The second prompt is optional, and it's strongly believed that the first one is too.

The second prompt may represent a "cross-reference" of some sort.

### `DATAPUT0%`

Put to data model.

 `DATAPUT0%`
 `DATAPUT0% *<prompt1>*`
 `DATAPUT0% *<prompt1>*, *<prompt2>*`

It's not known exactly what this means.  It appears to be some sort of database.

See `DATARUN0%` for comments on the arguments.

### `DATAGET0%`

Get from data model.

 `DATAGET0%`
 `DATAGET0% *<prompt1>*`
 `DATAGET0% *<prompt1>*, *<prompt2>*`

It's not known exactly what this means.  It appears to be some sort of database.

See `DATARUN0%` for comments on the arguments.



## Miscellaneous instructions
### `SYSCALL`

Execute DOS command.

 `SYSCALL *<prompt>*`

It is believed that the `*<prompt>*` must be of string type.  Its contents are presumably executed as a straight system call.  It's not known if any return code is checked.

### `NEWPAGE`

End the current "page" of instructions.

 `NEWPAGE`
 `NEWPAGE *<x>*`

It is believed that this is used as a reference point for the `GOTO` command.  This behaviour is not fully understood; it may be a simple label.

It's not known what the meaning of `*<x>*` is.  Attested values are `0` and `1`.  If you do not specify this parameter, it will be assembled as `0`.

### `SAVEADDR`

Save address for return from subroutine, or something like that.

 `SAVEADDR`

BANCSTAR instruction `8400`.  Not very well understood.

### `AUTOSAVE`

Auto-save something.

 `AUTOSAVE`

BANCSTAR instruction `9001`.  Not very well understood.

### `AUTOSOLVE`

Auto-solve something.

 `AUTOSOLVE`

BANCSTAR instruction `8700`.  A mystery.

### `RAW`

Raw BANCStar command.

 `RAW *<integer>*, *<integer>*, *<integer>*, *<integer>*`

This simply assembles to the BANCStar command specified by the four integers given.

You can use this to work around limitations in the BANCNova assembly language.  The disassembler will put unrecognised instructions in this form.

If you find a BANCStar instruction which disassembles to this form, but you expected it to result in a proper assembly instruction, please file a bug.



## Bugs/Missing features
* The `SET` command is currently based on a slightly misunderstanding of the BANCStar arithmetic instruction.  This will be corrected shortly.
* The conditional operators used in `COND` etc turn out to depend in various ways on the *type* of the prompt cell.  This is currently not represented, and we always show the numeric-style operator: can be misleading if you're working on a string!
* missing syntax for describing prompt cells themselves.
* missing syntax for describing "forms" (we don't understand them yet).
* missing syntax for making `SHOW` coordinates simpler (maybe skip this??)
* missing syntax for "page conditionals" (nobody even understands these)
* missing syntax for file commands (not very well understood)
* missing syntax for "draw" commands (not understood)
