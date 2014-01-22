mcl-project-manager
===================

An old project manager UI for Macintosh Common LISP 3.0. I built this
primarily in college, to give MCL a CodeWarrior-like project manager
IDE window. It's built for Mark Kantrowitz's DEFSYSTEM build system.

![project manager window](https://raw2.github.com/aalpern/mcl-project-manager/master/screenshots/project-window1.png)

The original README follows:

```
Defsystem Project Manager
version 1.0
July ??, 1996

Copyright © 1995, 1996 Adam Alpern
alpern@brightware.com

The Project Manager was written to work with Mark Kantrowitz's DEFSYSTEM
utility. At the moment, it does not work with any other defsystem-type utility.
If there is demand and I have time it may eventually support defunit and the
various other defsystems out there. Included are copies of defsystem 2.5 and
3.0. I still use 2.5, and haven't extensively tested version 3.0, so your mileage
may vary. The Project Manager, to the best of my knowledge, works with either one,
although 3.0 and logical directories don't get along so well.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

REQUIREMENTS

This release (1.0) requires the latest version of UI-Lib (1.0). If you
are using an older version, download UI-Lib 1.0. See the WHERE TO GET IT
section below for where to pick up a copy.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INSTALLATION AND LOADING

Project manager requires my UI-Lib library of MCL interface code, which is
available at the same places the project manager is. UI-Lib and project manager
both come with a defsystem file. You will have to edit the pathnames therein
for your system, and then place the .system files in the directory pointed to
by make::*central-registry*.

Compile and load both systems with this form:

(progn
  (oos :ui-lib-1.0 :load)
  (oos :project-manager-1.0 :load))

I load these systems (along with other modifications) and then save an
image with my custom environment. If you do this, you need to specify the
resources for the project manager, which you can do like this:

(save-application "MCL/custom"
                  :init-file nil
                  :clear-clos-caches t
                  :resources (projman-resources))

If you want to use the project manager right away, then load the resources
immediately:

(with-open-resource-file (f *project-manager-rsrc-file*)
  (init-projman-resources))
(setup-project-menu)

The function #'setup-project-menu creates a new "Projects" menu with a menu
item for each .system file in make::*central-registry*. Choosing an item will
open a project manager window for that system, and let you change defsystem
options.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ACKNOWLEDGEMENTS

Thanks to Kai Zimmerman and David B. Lamkins for bug fixes and enhancements.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TO DO

¥ save window size when doing a window collapse/expand
¥ save children's expanded/collapsed state when collapsing/expanding parent
¥ cache subviews when expanded so they're only generated on the first expansion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WHERE TO GET IT

The Project Manager homepage is at:
http://hampshire.edu/~adaF92/MCL/Project-Manager.html

The latest version is always at:
http://hampshire.edu/~adaF92/MCL/dist/project-manager-latest.sit.hqx

You can get the latest UI-lib from:
http://hampshire.edu/~adaF92/MCL/dist/ui-lib-latest.sit.hqx

And, of course, from
ftp://ftp.digitool.com/pub/mcl/contrib/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

REVISION HISTORY

ala = Adam Alpern
dbl = David B. Lamkins

¥ v1.0, 07/??/96
	- This release is not compatible with MCL 2.0. It does work in MCL 3.0
	  and MCL-PPC 3.9.
	- Modified to work with 1.0 release of UI-Lib, which is required.
	- #'systems function now correctly parses .system filenames with
	  more than one period character in them.
	- revised systems-dialog. It's now more compact, and resizable.
	  Uses *systems-dialog-default-position* and *systems-dialog-default-size*
	- path-editor.lisp had a corrupted resource fork. Nuked it.

¥ v0.7, 04/5/96
	- component-views may now have an expanded-icon, if they are expandable
	  (take a look at an opened module icon - the folder opens now)
	- uses new, multi-paned preferences-dialog
	- double-click on either info bar in a project-manager-window to collapse
	  it, again to expand (poor-man's window-shade). Saves space on small monitors.
	  Doesn't save old size though.
	- merged in DBL's systems-dialog. If you want to save space in
	  the menubar, set *project-manager-use-menu* to nil before calling
	  setup-project-menu.  This will add a "Systems" item to the "Tools"
	  menu, which opens a dialog listing the available systems.
	- added "Update Window" command to the commands menu.
	- probably more changes, but I can't remember them
	- require UI-lib 0.7

¥ v0.6.2, 12/10/95
	- dbl: more changes to defsystem.lisp (v.3.0)
	- dbl: See component-views.lisp changes for 12/09/95.
	- ala: removed unused icons from resource file.

¥ v0.6.1, 12/6/95
	- forgot to change *projman-color-p* to *project-manager-color-p*
	  in new-dialog.lisp. Fixed.
	- includes defsystem.lisp v3.0, modified by David Lamkins to work
	  in MCL 3.0.

¥ v0.6, 12/5/95
	- included David Lamkin's system-dialog
	- created a defsystem sub-directory, added defsystem-patches.lisp
	  and search-systems.lisp to the distribution.
	- added an About box, changes to the menu (color-p item, more
	  logical updating)
        - Incorporates changes by David B. Lamkins.
	  See component-views.lisp, manage-system-ccl3.lisp, and menu.lisp.

¥ v0.5.1, 11/10/95. Bug fixes.
	- Thanks to Kai Zimmerman for finding and fixing a bug that
	  caused a stack overflow if you clicked in the lock-region

¥ v0.5, 10/4/1995
	- First public release.
```
