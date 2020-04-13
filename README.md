# hspaint
![example1](https://raw.githubusercontent.com/ksadov/hspaint/pointToLine/carol1.png)

[![ksadov](https://circleci.com/gh/ksadov/hspaint.svg?style=svg)](https://app.circleci.com/pipelines/github/ksadov/hspaint)

#### To run: ####

```
stack run [path to optional .config file]
```

To load an existing file, type "y" at the prompt and then enter the name of the file that you wish to load at the second prompt. If you don't want to load an existing file, type "n" and then enter the file name that you'd like to save your new drawing under. The canvas will be displayed in fullscreen mode. To draw, drag the cursor over the canvas while holding the left mouse key.

### .config ###

.config files specify height, width, and color palettes. You can specify a maximum of 10 colors per palette. Color 0 will be set as the background color. If no .config file is specified as an argument, the image will be loaded with the default configuration: 150 x 100 pixels, white background, with a palette of primary colors.

### controls ###

`Esc` exit without saving

`s` save image in .png format

`q` decrease brush size

`w` increase brush size

`0-9` switch brush color

`e` eraser; switch brush color to background color (the same effect as pressing the `0` key)

`Shift + 0-9` switch dithering pattern

`h` flip canvas horizontally

`m` toggle multiply mode

`c` clear canvas
