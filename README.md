# Translate Config Files
Translate config files between the pattern
```
[Section1]
setting=value
```
which we call `Sections` and the pattern
```
Section1.setting=value
```
which we call `Simple`.

In order to translate from `Sections` to `Simple` use
```
runghc TranslateConfigFiles/SectionsToSimple.hs Tests/data/sections.conf Tests/data/simpleFromSections.conf
```
To translate from `Simple` to `Sections` use
```
runghc TranslateConfigFiles/SimpleToSections.hs Tests/data/simple.conf Tests/data/sectionsFromSimple.conf
```
