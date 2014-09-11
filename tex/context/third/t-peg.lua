-- PEG Typesetting Module for ConTeXt
-- Copyright (C) 2014 John Stewart
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

if not modules then
   modules = { }
end
modules['t-peg'] = {
   version = "1.0.0",
   comment = "LuaTeX script for the t-peg ConTeXt module.",
   author = "John Stewart",
   copyright = "2014 John Stewart",
   license = "GNU GPL 3"
}

------------------------------------------------------------------------------------------------------------------------
-- Formatting functions
------------------------------------------------------------------------------------------------------------------------
local formatters = { }

function formatters.texescape(s)
   s = s:gsub("{", "\\{")
   s = s:gsub("}", "\\}")
   s = s:gsub("\\([^{}&\\])", "\\textbackslash{}%1")
   s = s:gsub("\\\\", "\\textbackslash\\textbackslash{}")
   s = s:gsub("\\$", "\\textbackslash{}")
   s = s:gsub("%%", "\\%%")
   s = s:gsub("&", "\\letterampersand{}")
   s = s:gsub("%$", "\\$")
   return s
end

function formatters.Grammar(content)
   defs = ""
   for i, v in ipairs(content) do
        defs = defs .. v
   end
   return
      string.format("\\namedpegparameter{\\currentpeg}{grammarstartcommand}\n%s\\pegparameter{grammarstopcommand}\n",
                    defs)
end

function formatters.Definition(nonterminal, definition)
   nonterminal = string.format("\\namedpegparameter{\\currentpeg}{definitionntcommand}{%s}", nonterminal)
   return string.format("\\namedpegparameter{\\currentpeg}{definitioncommand}{%s}{%s}\n", nonterminal, definition)
end

function formatters.Expression(first, rest)
   output = first
   for i, v in ipairs(rest) do
      output = string.format("\\namedpegparameter{\\currentpeg}{choicecommand}{%s}{%s}", output, v)
   end
   return output
end

function formatters.Prefix(prefix, primary)
   if primary == nil then
      primary = prefix
      prefix = nil
   end
   if prefix ~= nil then
      if prefix == "&" then
         prefix = "\\&{}"
      end
      primary = string.format("\\namedpegparameter{\\currentpeg}{prefixcommand}{%s}", prefix) .. primary
   end
   return primary
end

function formatters.Suffix(primary, suffix)
   if suffix ~= nil then
      primary = primary .. string.format("\\namedpegparameter{\\currentpeg}{suffixcommand}{%s}", suffix)
   end
   return primary
end

function formatters.Sequence(prefixes)
   for i, v in ipairs(prefixes) do
      if i == 1 then
         output = string.format(" %s ", v)
      else
         output = string.format("\\namedpegparameter{\\currentpeg}{sequencecommand}{%s}{%s}", output, v)
      end
   end
   return output
end

function formatters.Group(sequence)
   return string.format("\\namedpegparameter{\\currentpeg}{groupcommand}{%s}", sequence)
end

function formatters.Literal(literal)
   return string.format("\\namedpegparameter{\\currentpeg}{literalcommand}{%s}", literal)
end

function formatters.Class(content)
   output = ""
   for i, v in ipairs(content) do
      output = output .. v
   end
   return string.format("\\namedpegparameter{\\currentpeg}{classcommand}{%s}", output)
end

function formatters.Range(low, high)
   if high ~= nil then
      return string.format("\\namedpegparameter{\\currentpeg}{rangecommand}{%s}{%s}", low, high)
   else
      return string.format("\\namedpegparameter{\\currentpeg}{simplerangecommand}{%s}", low)
   end
end

function formatters.Description(text)
   return string.format("\\namedpegparameter{\\currentpeg}{descriptioncommand}{%s}", text)
end

function formatters.NonTerminal(symbol)
   return string.format("\\namedpegparameter{\\currentpeg}{identifiercommand}{%s}", symbol)
end

function formatters.ComplexChar(code, description)
   return string.format("\\namedpegparameter{\\currentpeg}{complexcharcommand}{%s}{%s}", code, description)
end

function formatters.RAW(identifier)
   return identifier
end

function formatters.DOT(...)
   return "\\namedpegparameter{\\currentpeg}{dotcommand}"
end

function formatters.SimpleChar(char)
   return formatters.texescape(char)
end

function formatters.LineFeed(...)
   return "\\namedpegparameter{\\currentpeg}{linefeedcommand}"
end

------------------------------------------------------------------------------------------------------------------------
-- PEG for the PEG text
------------------------------------------------------------------------------------------------------------------------
local ntGrammar = lpeg.V("Grammar")
local ntDefinition = lpeg.V("Definition")

local ntExpression = lpeg.V("Expression")
local ntSequence = lpeg.V("Sequence")
local ntPrefix = lpeg.V("Prefix")
local ntSuffix = lpeg.V("Suffix")
local ntPrimary = lpeg.V("Primary")
local ntNonTerminal = lpeg.V("NonTerminal")
local ntGroup = lpeg.V("Group")

local ntIdentifier = lpeg.V("Identifier")
local ntIdentStart = lpeg.V("IdentStart")
local ntIdentCont = lpeg.V("IdentCont")

local ntLiteral = lpeg.V("Literal")
local ntLiteralSingle = lpeg.V("LiteralSingle")
local ntLiteralDouble = lpeg.V("LiteralDouble")
local ntClass = lpeg.V("Class")
local ntRange = lpeg.V("Range")
local ntDescription = lpeg.V("Description")
local ntLineFeed = lpeg.V("LineFeed")

local ntChar = lpeg.V("Char")
local ntCharEscape = lpeg.V("CharEscape")
local ntComplexChar = lpeg.V("ComplexChar")
local ntSimpleChar = lpeg.V("SimpleChar")

local ntLEFTARROW = lpeg.V("LEFTARROW")
local ntSLASH = lpeg.V("SLASH")
local ntAND = lpeg.V("AND")
local ntNOT = lpeg.V("NOT")
local ntQUESTION = lpeg.V("QUESTION")
local ntSTAR = lpeg.V("STAR")
local ntPLUS = lpeg.V("PLUS")
local ntOPEN = lpeg.V("OPEN")
local ntCLOSE = lpeg.V("CLOSE")
local ntDOT = lpeg.V("DOT")

local ntSpacing = lpeg.V("Spacing")
local ntSpace = lpeg.V("Space")
local ntComment = lpeg.V("Comment")
local ntEndOfLine = lpeg.V("EndOfLine")
local ntEndOfFile = lpeg.V("EndOfFile")

local grammar = lpeg.P{
   "Grammar";
   Grammar = ntSpacing * lpeg.Ct(ntDefinition^1) * ntEndOfFile / formatters.Grammar,
   Definition = lpeg.Cs(ntIdentifier) * ntLEFTARROW * lpeg.Cs(ntExpression) / formatters.Definition,

   Expression = lpeg.Cs(ntSequence) * lpeg.Ct((ntSLASH * lpeg.Cs(ntSequence))^0) / formatters.Expression,
   Sequence = lpeg.Ct(lpeg.Cs(ntPrefix)^0) / formatters.Sequence,
   Prefix = lpeg.Cs(ntAND + ntNOT)^-1 * lpeg.Cs(ntSuffix) / formatters.Prefix,
   Suffix = lpeg.Cs(ntPrimary) * lpeg.Cs(ntQUESTION + ntSTAR + ntPLUS)^-1 / formatters.Suffix,
   Primary = ntLineFeed + ntNonTerminal + ntGroup + ntLiteral + ntClass + ntDescription + ntDOT,
   NonTerminal = lpeg.Cs(ntIdentifier) * -ntLEFTARROW / formatters.NonTerminal,
   Group = ntOPEN * lpeg.Cs(ntExpression) * ntCLOSE / formatters.Group,

   -- Lexical syntax
   Identifier = lpeg.Cs(ntIdentStart * ntIdentCont^0) * ntSpacing / formatters.RAW,
   IdentStart = lpeg.R("az", "AZ", "__"),
   IdentCont = ntIdentStart + lpeg.R("09") + lpeg.P("-"),

   Literal = lpeg.Cs(ntLiteralSingle + ntLiteralDouble) * ntSpacing / formatters.Literal,
   LiteralSingle = lpeg.P("'") * lpeg.Cs((ntChar - lpeg.P("'"))^0) * lpeg.P("'") / formatters.RAW,
   LiteralDouble = lpeg.P("\"") * lpeg.Cs((ntChar - lpeg.P("\""))^0) * lpeg.P("\"") / formatters.RAW,
   Class = lpeg.P("[") * lpeg.Ct(lpeg.Cs(ntRange - lpeg.P("]"))^0) * lpeg.P("]") * ntSpacing / formatters.Class,
   Range = ((lpeg.Cs(ntChar) * lpeg.P("-") * lpeg.Cs(ntChar)) + lpeg.Cs(ntChar)) / formatters.Range,
   Description = lpeg.P("<") * lpeg.Cs((lpeg.P("\\\\") + lpeg.P("\\>") + lpeg.P(1) - lpeg.P(">"))^0) * lpeg.P(">") *
      ntSpacing / formatters.Description,
   LineFeed = lpeg.P("\\n") * ntSpacing / formatters.LineFeed,

   Char = ntComplexChar + ntSimpleChar,
   CharEscape = lpeg.P("\\"),
   SimpleChar = ((ntCharEscape * lpeg.R("nn", "rr", "ty", "''", "\"\"", "[[", "]]", "\\\\")) +
         (ntCharEscape * lpeg.R("02") * lpeg.R("07") * lpeg.R("07")) +
         (ntCharEscape * lpeg.R("07") * lpeg.R("07")) +
         (lpeg.P(1) - ntCharEscape)) / formatters.SimpleChar,
   ComplexChar = ntCharEscape *
      lpeg.P("x{") * lpeg.Cs(lpeg.P(1)^-4) * lpeg.P(":") * lpeg.Cs((lpeg.P(1) - lpeg.P("}"))^0) *
      lpeg.P("}") / formatters.ComplexChar,

   LEFTARROW = lpeg.P("<-") * ntSpacing,
   SLASH = lpeg.P("/") * ntSpacing,
   AND = lpeg.Cs(lpeg.P("&")) * ntSpacing,
   NOT = lpeg.Cs(lpeg.P("!")) * ntSpacing,
   QUESTION = lpeg.Cs(lpeg.P("?")) * ntSpacing,
   STAR = lpeg.Cs(lpeg.P("*")) * ntSpacing,
   PLUS = lpeg.Cs(lpeg.P("+")) * ntSpacing,
   OPEN = lpeg.P("(") * ntSpacing,
   CLOSE = lpeg.P(")") * ntSpacing,
   DOT = lpeg.Cs(lpeg.P(".")) * ntSpacing / formatters.DOT,

   Spacing = (ntSpace + ntComment)^0,
   Space = lpeg.P(" ") + lpeg.P("\t") + ntEndOfLine,
   Comment = lpeg.P("#") * (lpeg.P(1) - ntEndOfLine)^0 * ntEndOfLine,
   EndOfLine = lpeg.P("\r\n") + lpeg.P("\n") + lpeg.P("\r"),
   EndOfFile = -lpeg.P(1)
} * -1

------------------------------------------------------------------------------------------------------------------------
-- Main entry-point for command
------------------------------------------------------------------------------------------------------------------------
thirddata = thirddata or { }
thirddata.peg = thirddata.peg or { }
function thirddata.peg.typeset(s)
   s = s .. "\n"
   output = grammar:match(s)
   if output == nil then
      error("Error in Parsing Expression Grammar syntax.")
   else
      return context(output)
   end
end
