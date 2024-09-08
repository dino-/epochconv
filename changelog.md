1.6 (2024-09-04)

  * Switched from hpack to cabal file
  * Changed version to 1.6
  * Adjusted for parsing format for RFC5322 to handle zero- or space-padded day-of-month

1.5 (2024-08-05)

  * Removed license end-date
  * Changed version to 1.5
  * Changed all mention of ISO1601 to ISO8601
  * Added notice that this project is a script
  * Using First Monoid instead of foldl mplus
  * Removed Setup.hs and TODO files
  * Added back GHC option -fno-warn-type-defaults
  * Moved the epochconv.hs source file into the project root
  * Put the #! script start block back


1.4 (2021-11-29)

  * Merged recent changes from scripts/epochconv.hs
  * Made all indenting a uniform 2-spaces
  * Changed version to 1.4
  * Put the #! script start block back
  * Moved the source file into the project root
  * Removed Setup.hs and TODO files
  * Moved copyright up to 2021
  * Switched to a proper hsinstall dir


1.0 (2014-09-01)

  * Initial commit
  * Project set-up
  * Added version number to usage output
  * Removed #! line from source file
  * Switched from darcs to git
  * Switched licensing from BSD3 to ISC
  * Added generated files for AppImage creation
