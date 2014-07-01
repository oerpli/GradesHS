GradesHS
========

Tool for tracking your grades.


### Usage
Compile and run `main :: IO ()` (or use the binary) and open `localhost:9999` in your browser.
The `css` folder should be in the same directory as the binary (if you want to change that behaviour you have to change it in the source).

On the left side there are the currently tracked subjects - the list is initalized with default data - if you don't want to remove it every time change the source code or wait for the save update.

Enter the data in the input fields and press the nearby `+`. If you mess up the format the application will crash.
If you want to remove a subject press the upper `-`. If you want to remove an exam from a subject press the lower  `-`.

1 is the best grade. 5 is negative. "+" is neutral and does not influence the average.

### Planned:
- save/load from json
- international (at least a little bit) grade support
- advanced statistics (plots etc)



#### Deprecated:
In `./src/grades-console.hs` there is some old stuff from a terminal version - it's more or less compatible with the current build but not longer maintained.

- `list`: Lists all subjects added so far
- `mod`:  Modify subjects
  - `add`:  Add new subject
  - `res`:  Add result to existing subject
  - `rem`:  Remove someting
    - `sub`:  Remove subject
    - `res`:  Remove result

