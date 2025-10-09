- DONE work out data flow for updating state based on input

- DONE draw circle around last mouse click

- DONE centre the circle

- push SDL config into Reader monad
  - reader and state
  - so "load font" can load path+font name from reader

- DONE fix aspect ratio
- DONE add info bar
- NO no drawing in info bar
- DONE create buttons in info bar
- DONE pull drawRect up to UI layer as a Drawable
- DONE have Scene.mk persist the ui
  - how do we draw different button state?

- DONE why does button only work once
  - YES maybe a debug logging issue?

- DONE have "last pressed button" state
- DONE controls whether click draws square or circle 
- keyboard shortcuts (1/2) for buttons in info bar
- accumulate drawables
- snap drawables to grid
- second click removes drawable
  - use grid as input grid

