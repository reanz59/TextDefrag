# TextDefrag
Text extracted from documents sometimes consists of many word fragments.

The degree of fragmentation varies depending on which text extractor is used. 
So far, the xpdfreader: https://www.xpdfreader.com/ results are better than tika.

For example: "ISO’s por t fo l io of over 18 500* standards provides business, government and society with practical tools
for a l l three dimensions of sustainable development: economic, env ironmental and socia l."

This project provides two ways of handling fragmented text: ASCII, or UNICODE. For development of defragmenting just use the defragment.lisp code.

Alternatively, in addition to defragment.lisp, use the systems.lisp to use UNICODE code-points.



