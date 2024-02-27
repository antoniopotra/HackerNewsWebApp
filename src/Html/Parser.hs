module Html.Parser where

import Html
import Parser qualified as P
import Result
import Text.Read (Lexeme (String))

makeTuple :: a -> b -> (a, b)
makeTuple p1 p2 = (p1, p2)

-- | Parses HTML text, consuming input as long as it doesn't contain the characters < or >
--
-- >>> P.parse text "some text"
-- Success "some text"
--
-- >>> P.parse text "!@#$%^&&*()"
-- Success "!@#$%^&&*()"
--
-- >>> P.runParser text "some text<"
-- Success ("some text","<")
--
-- >>> P.runParser text "<some text>"
-- Error (UnexpectedInput {gotInput = "<some text>", expectedInput = "At least one character"})
text :: P.Parser String
text = P.some (P.satisfies (\c -> c /= '<' && c /= '>') "text")

test :: P.Parser (Int, [Int])
test = P.number `P.andThen` P.some (P.char ',' `P.pThen` P.number)

-- | Parses a self closing tag
--
-- A self closing tag is an identifier followed by a possibly empty list of attributes (see @attributes@) between < and />
--
-- Some useful functions:
-- - @Parser.ident@
-- - @Parser.between@
-- - @Parser.tag@
-- - @Parser.char@
--
-- >>> P.parse selfClosing "<a/>"
-- Success ("a",[])
--
-- >>> P.parse selfClosing "<a>"
-- Error (UnexpectedInput {gotInput = ">", expectedInput = "character '/'"})
--
-- >>> P.parse selfClosing "<a x=\"y\"/>"
-- Success ("a",[("x",Just "y")])
--
-- >>> P.parse selfClosing "<a x/>"
-- Success ("a",[("x",Nothing)])
selfClosing :: P.Parser (String, [(String, Maybe String)])
selfClosing = P.between (P.char '<') (P.tag "/>") (P.pMap2 makeTuple P.ident (P.ws `P.pThen` attributes))

-- | Parses an opening tag
--
-- A tag is an identifier followed by a possibly empty list of attributes (see @attributes@) between < and >
--
-- Note: There might be whitespace between the identifier and >
--
-- Some useful functions:
-- - @Parser.between@
-- - @Parser.tag@
-- - @Parser.char@
--
-- >>> P.parse openTag "<a>"
-- Success ("a",[])
--
-- >>> P.parse openTag "<a >"
-- Success ("a",[])
openTag :: P.Parser (String, [(String, Maybe String)])
openTag = P.between (P.char '<') (P.char '>') (P.pMap2 makeTuple P.ident (P.ws `P.pThen` attributes))

-- | Parses a possibly empty list of attributes
--
-- An attribute is a key, optionally followed by = and a value between double quotes or single qoutes.
-- The attributes are separated by whitespace.
--
-- Some useful functions:
-- - @Parser.sepBy@
-- - @Parser.between@
-- - @Parser.opt@
-- - @Parser.orElse@
--
-- >>> P.parse attributes "a=\"b\""
-- Success [("a",Just "b")]

--
-- >>> P.parse attributes "a='b'"
-- Success [("a",Just "b")]
--
-- >>> P.parse attributes "a=\"\""
-- Error (ExpectedEOF {remainingInput = "=\"\""})
--
-- >>> P.parse attributes "a b=\"x\""
-- Success [("a",Nothing),("b",Just "x")]
--
-- Success [("a",Nothing),("b",Just "x")]
-- >>> P.parse attributes "a b"
-- Success [("a",Nothing),("b",Nothing)]
--
-- >>> P.parse selfClosing "<img source=\"some_path\" width=\"100px\"/>"
-- Success ("img",[("source",Just "some_path"),("width",Just "100px")])
attributes :: P.Parser [(String, Maybe String)]
attributes = P.sepBy P.ws attribute
  where
    attribute :: P.Parser (String, Maybe String)
    attribute = P.pMap2 makeTuple P.ident value

    value :: P.Parser (Maybe String)
    value = P.opt (P.char '=' `P.pThen` quotedValue)

    quotedValue :: P.Parser String
    quotedValue = P.between (P.char '\"') (P.char '\"') anything `P.orElse` P.between (P.char '\'') (P.char '\'') anything

    anything :: P.Parser String
    anything = P.some (P.satisfies (\c -> c /= '\"' && c /= '\'') "anything")

-- | Parses the given closing tag
--
-- A closing tag is an identifier between </ and >
--
-- Note: There might be whitespace between the identifier and >
--
-- Some useful functions:
-- - @Parser.tag@
--
-- >>> P.parse (closingTag "a") "</a>"
-- Success ()
-- >>> P.parse (closingTag "a") "</a  >"
-- Success ()
-- >>> P.parse (closingTag "a") "</div>"
-- Error (UnexpectedInput {gotInput = "d", expectedInput = "character 'a'"})
closingTag :: String -> P.Parser ()
closingTag tag = P.pMap (const ()) $ P.tag "</" `P.pThen` P.tag tag `P.pThen` ((P.ws `P.pThen` P.char '>') `P.orElse` P.char '>')

-- | Run a parser between HTML tags, checking that the opening and closing tag match.
--
-- Some useful functions:
-- - @openTag@
-- - @closingTag@
-- - @Parser.pWith@
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a>x</a>"
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a>x</b>"
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a y=\"z\">x</a>"
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a y>x</a>"
--
-- >>> P.parse (betweenHtmlTags (P.char 'b')) "<a>b</a>"
--
-- >>> P.parse (betweenHtmlTags (P.char 'b')) "<a x=\"y\">b</a>"
betweenHtmlTags :: P.Parser a -> P.Parser (a, String, [(String, Maybe String)])
betweenHtmlTags p = P.pWith openTag $ \(tagName, tagAttrs) ->
  P.pWith p $ \result ->
    P.pWith (closingTag tagName) $ \_ ->
      P.succeed (result, tagName, tagAttrs)

-- | Parses a HTML node
--
-- A HTML node is one of:
-- - a self closing tag
-- - an opening and closing tag, with more HTML between them
--
-- Some useful functions:
-- - @Parser.pMap@
-- - @Parser.orElse@
-- - @html@
-- - @betweenHtmlTags@
-- - @selfClosing@
--
-- >>> P.parse htmlNode "<a/>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = []})
--
-- >>> P.parse htmlNode "<a>b</a>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = [b]})
--
-- >>> P.parse htmlNode "<a><b>text</b></a>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = [<b>
--   text
-- </b>]})
htmlNode :: P.Parser HtmlNode
htmlNode = selfClosingNode `P.orElse` openCloseNode
  where
    selfClosingNode :: P.Parser HtmlNode
    selfClosingNode = P.pMap (\(tag, attrs) -> HtmlNode tag attrs []) selfClosing

    openCloseNode :: P.Parser HtmlNode
    openCloseNode = P.pMap (\(children, tag, attrs) -> HtmlNode tag attrs children) (betweenHtmlTags (P.many html))

-- | Parses a HTML node or a text node
--
-- Some useful functions:
-- - @Parser.pMap@
-- - @Parser.orElse@
-- - @htmlNode@
-- - @text@
--
-- >>> P.parse html "<a y=\"z\"><b>x</b></a>"
-- WAS Success <a y="z">
-- WAS  <b>
-- WAS    x
-- WAS  </b>
-- WAS </a>
-- NOW Success <a y="z">
--   <b>
--     x
--   </b>
-- </a>
--
-- >>> P.parse html "<a y=\"z\"><b>x <z></z></b></a>"
-- WAS Success <a y="z">
-- WAS  <b>
-- WAS    x
-- WAS    <z/>
-- WAS  </b>
-- WAS </a>
-- NOW Success <a y="z">
--   <b>
--     x
--     <z/>
--   </b>
-- </a>
-- >>>  P.parse html "<div>Some <b>bold</b> text</div>"
-- Success <div>
--   Some
--   <b>
--     bold
--   </b>
--    text
-- </div>
html :: P.Parser Html
html = P.pMap Text text `P.orElse` P.pMap Node htmlNode

document :: P.Parser Document
document = P.eof $ P.pMap Document $ P.some html

parse :: String -> Result P.ParseError Document
parse = P.parse document

-- >>> parseNode "<div></div>"
-- Success (HtmlNode {nodeTag = "div", nodeAttrs = [], nodeChildren = []})
--
-- >>> parseNode "<div></div><p></p>"
-- Error (ExpectedEOF {remainingInput = "<p></p>"})
parseNode :: String -> Result P.ParseError HtmlNode
parseNode = P.parse (P.eof htmlNode)
