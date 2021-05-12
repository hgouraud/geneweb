# Carrousel

Other_fn is a GeneWeb v7 plugin to allow display other first names.

## Installation (for users)

Activate the plugin mechanism at gwd launch time with
```
gwd ... -plugins path_to_plugins
```
This is probably already done as several v7 functions are provided
through a plugin mechanism (cgl, export, xhtml, ...)

Add otherfn_p to the active plugin list in your .gwf file:

```
plugins=cgl,export,forum,no_index,xhtml,otherfn_p
```

## Operations

Otherfn_p is activated when clicking on the first name of a person.
At first, only the exact match for the first name are displayed.
Clicking on the "thumbs up" icon will display other first names that
may contain the requested first name.
"Contains" can be adjusted with exact (=), or word boundary options.
In exact mode, capitals and accents are taken into account.
In word boundary mode, "henri" will not return "henriette".

## Copyright

Plugin written by H Gouraud 
