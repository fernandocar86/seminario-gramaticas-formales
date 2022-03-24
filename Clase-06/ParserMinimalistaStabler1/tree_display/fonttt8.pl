/* fonttt8.pl
 *
 * Font width tables for tk_tree.pl
 *
 * font_width(+FontName, +String, ?PixelWidth) iff the width of String
 *  in FontName is PixelWidth.
 */

:- module(fonttt8, [label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(library(charsio), [format_to_chars/3]).

tk_font_name('-adobe-courier-bold-r-normal--8-80-*'). % approximate!!
tk_geometry('wm geometry . 380x428-5+40').

label_size(Label, PixWidth, 8) :- 
%	format_to_chars("~p", [Label], LabelChars),
	sformat(LabelString,'~p',[Label]), string_to_list(LabelString,LabelChars),
	chars_width(LabelChars, 0, Width0, tt8),
	PixWidth is Width0/100000.

chars_width([], Width, Width, _).
chars_width([C|Cs], Width0, Width, Font) :-
	char_width(Font, C, Width1),
	Width2 is Width0+Width1,
	chars_width(Cs, Width2, Width, Font).

char_width(tt8, _,500000).
