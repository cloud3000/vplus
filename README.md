### One day back in 2010.

I decided to create a multi-platform version of VIEW/3000

I had already developed software to migrate Image/3000 to SQL, so it only makes since to have the front-end UI on the same platform.

1. Reverse engineer the HP Formfile format, extract the forms meta-data from the forms file.
2. Transfer the meta-data to the new platform.
3. Load the meta-data into a Sqlite database file, with the extension `.vform`
4. Rewrite all the v/plus functions, AKA:Subroutine Library.

> Here are all the `v/plus` functions

These v/plus functions use the meta-data in the Sqlite DB to do basically the same thing they did on the HP3000, but GUI and Web/HTML. This version uses SP2 from Flexus to do the actual GUI/Web.

These v/plus functions here will not compile, they are missing dependencies.

1. TCL scripts that are used to do the field processing specs, and to read the Sqlite DB are not included.
2. Copylib modules, also not included.
3. SP2 from [Flexus](https://flexus.com/) also not included.

These routines were written for the Acu-Cobol compiler, but should compile with minor mods using the GNU-Cobol compiler.

My intension was to one day re-write these in ansi C, not C++, ansi C.

I will probably include the copylib modules here soon os at least they'll compile. But they still need the Tcl script and the SP2 runtime.

I'll ask my friend who created the Tcl scripts if he would like to upload them here as well.

However SP2 (the engin underneath it all) is proprietary and must be purchased.
