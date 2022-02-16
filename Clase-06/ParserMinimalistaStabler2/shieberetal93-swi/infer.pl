/*==========================================================

     Parser Based on a General Tabular Inference Engine

==========================================================*/

/*----------------------------------------------------------
                         LIBRARIES
----------------------------------------------------------*/

:- ensure_loaded(readin).            % provides:  read_in/1
:- use_module(library(lists)).       % provides:  append/3
                                     %            reverse/2


/*----------------------------------------------------------
                         COMPONENTS
----------------------------------------------------------*/

:- ensure_loaded(input).
:- ensure_loaded(driver).
:- ensure_loaded(items).
:- ensure_loaded(inference).
:- ensure_loaded(grammars).
:- ensure_loaded(utilities).
:- ensure_loaded(monitor).
% added timing predicates: Frank Morawietz [26/10/99]
:- ensure_loaded(timing).
