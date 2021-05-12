# Carrousel

Doc is a GeneWeb v7 plugin to allow access to .pdf, .html files and to sub-folders.

## Installation (for users)

Activate the plugin mechanism at gwd launch time with
```
gwd ... -plugins path_to_plugins
```
This is probably already done as several v7 functions are provided
through a plugin mechanism (cgl, export, xhtml, ...)

Add doc to the active plugin list in your .gwf file:
```
plugins=cgl,export,forum,no_index,xhtml,doc
```

## Operations

Doc is an extension of the m=IM and m=SRC commands.
m=DOC&s=filaneme.jpg is equivalent to m=IM;s=filename.jpg
m=DOC&s=filename.txt is equivalent to m=SRC;s=filename
m=DOC accepts extensions .pdf and .html, and allows sub folders as in
m=DOC&s=sub_folder/filename.jpg

.pdf and .html files are searched in the same folder as image files:
```src/basename/images```
Sub folders should be organised at this location.

## Copyright

Plugin written by H Gouraud 
