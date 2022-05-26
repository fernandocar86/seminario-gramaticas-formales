/* fontcmr10.pl
 *
 * Font width tables for latex_tree.pl
 *
 * font_width(+FontName, +String, ?PixelWidth) iff the width of String
 *  in FontName is PixelWidth.
 *
 * generated from tftopl -charcode-format=ascii cmr10.tfm cmr10.pl
 * 
 */

:- module(fontcmr10, [label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(library(charsio), [format_to_chars/3]).

% this font is not really available to tk
tk_font_name('-*-times-bold-r-normal--10-100-*').
tk_geometry('wm geometry . 380x428-5+40').

label_size(Label, PixWidth, 10) :- labelsize(Label, PixWidth, cmr10).

labelsize(Label, PixWidth, cmr10) :-
%	format_to_chars("~p", [Label], LabelChars),
	sformat(LabelString,'~p',[Label]), string_to_list(LabelString,LabelChars),
	chars_width(LabelChars, 0, Width0, cmr10),
	PixWidth is Width0/100000.

chars_width([], Width, Width, _).
chars_width([C|Cs], Width0, Width, Font) :-
	char_width(Font, C, Width1),
	Width2 is Width0+Width1,
	chars_width(Cs, Width2, Width, Font).

char_width(cmr10, C, Width) :- cmr10(C, Width), !.

cmr10(32, 277779).
cmr10(33, 277779).
cmr10(34, 500002).
cmr10(35, 833336).
cmr10(36, 500002).
cmr10(37, 833336).
cmr10(38, 777781).
cmr10(39, 277779).
cmr10(40, 277779).
cmr10(41, 277779).
cmr10(42, 500002).
cmr10(43, 777781).
cmr10(44, 277779).
cmr10(45, 333334).
cmr10(46, 277779).
cmr10(47, 500002).
cmr10(48, 500002).
cmr10(49, 500002).
cmr10(50, 500002).
cmr10(51, 500002).
cmr10(52, 500002).
cmr10(53, 500002).
cmr10(54, 500002).
cmr10(55, 500002).
cmr10(56, 500002).
cmr10(57, 500002).
cmr10(58, 277779).
cmr10(59, 277779).
cmr10(60, 277779).
cmr10(61, 777781).
cmr10(62, 472224).
cmr10(63, 472224).
cmr10(64, 777781).
cmr10(65, 750002).
cmr10(66, 708336).
cmr10(67, 722224).
cmr10(68, 763891).
cmr10(69, 680557).
cmr10(70, 652781).
cmr10(71, 784724).
cmr10(72, 750002).
cmr10(73, 361112).
cmr10(74, 51389).
cmr10(75, 777781).
cmr10(76, 625002).
cmr10(77, 916669).
cmr10(78, 750002).
cmr10(79, 777781).
cmr10(80, 680557).
cmr10(81, 777781).
cmr10(82, 736113).
cmr10(83, 555557).
cmr10(84, 722224).
cmr10(85, 750002).
cmr10(86, 750002).
cmr10(87, 1027781).
cmr10(88, 750002).
cmr10(89, 750002).
cmr10(90, 611113).
cmr10(91, 277779).
cmr10(92, 500002).
cmr10(93, 277779).
cmr10(94, 500002).
cmr10(95, 277779).
cmr10(96, 277779).
cmr10(97, 500002).
cmr10(98, 555557).
cmr10(99, 444446).
cmr10(100, 555557).
cmr10(101, 444446).
cmr10(102, 305557).
cmr10(103, 500002).
cmr10(104, 555557).
cmr10(105, 277779).
cmr10(106, 305557).
cmr10(107, 527781).
cmr10(108, 277779).
cmr10(109, 833336).
cmr10(110, 555557).
cmr10(111, 500002).
cmr10(112, 555557).
cmr10(113, 527779).
cmr10(114, 391668).
cmr10(115, 394445).
cmr10(116, 38889).
cmr10(117, 555557).
cmr10(118, 527781).
cmr10(119, 722224).
cmr10(120, 527781).
cmr10(121, 527781).
cmr10(122, 444446).
cmr10(123, 500002).
cmr10(124, 100003).
cmr10(125, 500002).
cmr10(126, 500002).
