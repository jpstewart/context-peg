# Module for Typesetting Parsing Expression Grammars in ConTeXt MkIV
## Requirements
You should have a complete installation of ConTeXt standalone or equivalent (e.g. a full installation via TeXLive). Only
ConTeXt MkIV is supported.

## Description
This is a third-party module for typesetting a parsing expression grammar (PEG) in a ConTeXt document. This module
defines an environment in which a parsing expression grammar may be written as per the grammar defined in Ford's
original PEG paper. When your document is compiled the grammar will typeset in an attractive and customizable way.

## Usage
To install, clone this repository or download an archive and extract it to either
$TEXMF/tex/texmf-modules/text/context/third/t-peg or $TEXMFHOME/context/third/t-peg. You can use it in your document
like so:

    \usemodule[peg]
    \starttext
      % Typeset a PEG
      \startpeg
        # Whitespace definitions
        Spacing <- " " / Newline
        Newline <- "\r\n" / [\r\n]

        # Grammar
        Operator <- [\-+*/]
        Number <- [0-9]+
        Operation <- Number (Operator Number)+
      \stoppeg

      % Place PEG as a float with caption
      \placepeg[][]{A sample grammar}
        {\startpeg
           Number <- [0-9]+
         \stoppeg}

      % Configuring a custom PEG style with emphasized non-terminals
      \def\nonterminal#1{\text{\em #1}}
      \definepeg[mypeg]
      \setuppeg[mypeg][definitionntcommand=\nonterminal,identifiercommand=\nonterminal]
    \stoptext

## Extensions
Several extensions to the standard PEG grammar are included for common requirements. A description
terminal has been added, which is in the format `<Descriptive text>` (that's with the angle brackets literally
included). This is by default rendered with emphasized (italicized) text surrounded by angle brackets. It is useful for
non-formal descriptions of a string match. An extra escape has been included for literals and ranges which describes a
Unicode character more attractively. To use this, use the form `\x{codepoint:description}`. For example, a line feed can
be described with `\x{000A:LINE FEED}`. This will be rendered by default with an italicized "U" followed by a
superscript code point and a subscript description. This is a more attractive way to describe specific characters than
the `\NNN` form included in the standard PEG grammar.

For more details and more examples see the generated documentation in the repository.
