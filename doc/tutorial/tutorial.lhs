<p>
  I've released <a href="http://hackage.haskell.org/package/xmlgen">xmlgen</a>
  to hackage just a few days ago. Xmlgen is a pure Haskell library with a convenient
  API for generating XML documents. It provides support for all functionality
  defined by the <a href="http://www.w3.org/TR/xml-infoset/">XML information set</a> and offers good performance and low memory usage.
  In our company,
  we developed xmlgen because we wanted the readability of XML literals
  (as for example provided by the <a href="http://hackage.haskell.org/package/hsp">hsp library</a>)
  without the drawbacks of a custom preprocessor (wrong line numbers in error messages,
  non-compositionality).
</p>

<p>
  In this blog article, I'll show you how to use the combinators provided
  by xmlgen to generate the following XML document:
</p>
<pre>
&lt;?xml version="1.0"?&gt;
&lt;people&gt;
  &lt;person age="32"&gt;Stefan&lt;/person&gt;
  &lt;person age="4"&gt;Judith&lt;/person&gt;
&lt;/people&gt;
</pre>

<p>
First, we import some modules:
</p>

> import Data.Monoid
> import qualified Data.ByteString.Lazy as BSL
> import Text.XML.Generator -- the main xmlgen module

<p>
Then we continue by generating the <tt>person</tt> element.
</p>

> genPersonElem :: (String, String) -> Xml Elem
> genPersonElem (name, age) =
>     xelem "person" $ xattr "age" age <#> xtext name

<p>
The <a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#v:xelem"><tt>xelem</tt></a>
combinator constructs an XML element from an element name
and from the children of the element. Xmlgen provides overloaded
variants of <tt>xelem</tt> to support a uniform syntax for
the construction of elements with qualified and unqualified names and
with different kinds of children.
The <a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#v:-60--35--62-"><tt>&lt;#&gt;</tt></a>
combinator separates the element's attributes from the other children (sub-elements
and text nodes).
The combinators <a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#v:xattr"><tt>xattr</tt></a>
and <a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#v:xtext"><tt>xtext</tt></a>
construct XML attributes and text nodes, respectively.
</p>

<p>
The result of an application of <tt>xelem</tt> has
type <tt><a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#t:Xml">Xml</a> <a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#t:Elem">Elem</a></tt>,
whereas <tt>xattr</tt> has result type <tt>Xml <a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#t:Attr">Attr</a></tt>.
This distinction is important so that attributes and elements can
not be confused. The result type of the <tt>xtext</tt> combinator
is <tt>Xml Elem</tt>; we decided against an extra type for text nodes
because for xmlgen's purpose text nodes and elements are almost interchangable.
</p>

<p>
The types <tt>Xml Elem</tt> and <tt>Xml Attr</tt> are both
instances of the <a
href="http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html"><tt>Monoid</tt></a>
type class. Constructing a list of elements from a list of persons and their
ages is thus quite easy:
</p>

> genPersonElems :: [(String, String)] -> Xml Elem
> genPersonElems = foldr mappend mempty . map genPersonElem

<p>
The pattern shown above is quite common, so xmlgen allows
the following shorthand notation using the
<a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#v:xelems"><tt>xelems</tt></a>
combinator.
</p>

> genPersonElems' :: [(String, String)] -> Xml Elem
> genPersonElems' = xelems . map genPersonElem

<p>
We are now ready to construct the final XML document:
</p>

> genXml :: Xml Doc
> genXml = let people = [("Stefan", "32"), ("Judith", "4")]
>          in doc defaultDocInfo $ xelem "people" (genPersonElems people)

<p>
For convenience, here is a standalone variant of the <tt>genXml</tt> function:
</p>

> genXml' :: Xml Doc
> genXml' =
>   let people = [("Stefan", "32"), ("Judith", "4")]
>   in doc defaultDocInfo $
>        xelem "people" $
>          xelems $ map (\(name, age) -> xelem "person" (xattr "age" age <#> xtext name)) people

<p>
Xmlgen supports various output formats through the overloaded
<a href="http://hackage.haskell.org/packages/archive/xmlgen/0.4.0.1/doc/html/Text-XML-Generator.html#v:xrender"><tt>xrender</tt></a> function. Here we render
to resulting XML document as a lazy byte string:
</p>

> outputXml :: IO ()
> outputXml = BSL.putStrLn (xrender genXml)

<p>
Loading the current file into ghci and evaluating <tt>outputXml</tt>
produces the following result:
</p>

<pre>
*Main&gt; outputXml
&lt;?xml version="1.0" encoding="UTF-8" standalone="yes"?&gt;
&lt;people
&gt;&lt;person age="32"
&gt;Stefan&lt;/person
&gt;&lt;person age="4"
&gt;Judith&lt;/person
&gt;&lt;/people
&gt;
</pre>

<p>
This blog post introduced most but not all features of the xmlgen API.
Check out the <a href="http://hackage.haskell.org/package/xmlgen">documentation</a>.
</p>

<p>
Happy hacking and have fun!
</p>
