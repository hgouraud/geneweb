# Carrousel

Places is a GeneWeb v7 plugin to display lists of persons associated to places.

## Installation (for users)

Activate the plugin mechanism at gwd launch time with
```
gwd ... -plugins path_to_plugins
```
This is probably already done as several v7 functions are provided
through a plugin mechanism (cgl, export, xhtml, ...)

Add places to the active plugin list in your .gwf file:
```
plugins=cgl,export,forum,no_index,xhtml,places
```

## Operations

The places plugin is available on the welcome page, or in the menubar depending on your 
choices driven by your .gwf file. See the plugin GUI documentation for details.

A set of buttons allow to isolate events for which the search should be made.
Places are listed in "short" or "long" format.

A place capture line allows to perform a search for a particular place,
with several restrictive or expansive options.

In both formats, surnames associated to each place are listed, with a number indicating
how many persons of this surname are concerned. Clicking on the number will
display the list of the concerned persons, with an indication of the type of event (NBMDS).
When this list is displayed, it is possible to ask fir a "relations tree" between
the persons of the list.
With the magic wand next to the place name, it is also possible to fall back into 
the places dictionary to perform some dictionary level correction.

WARNING: this plugin is strongly dependent on the syntax you have used to record places
and how systematic you have been in this effort.

## Copyright

Plugin written by H Gouraud 
