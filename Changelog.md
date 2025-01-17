# Changelog

## 0.2.1 → 0.2.2

* Enhancement: Faster model loading
* Bug fix: Place the camera and the light source further away from the origin.
* Bug fix: Specify 16GB dynamic space size for release builds.

## 0.2 → 0.2.1

* Enhancement: Add hotkeys C-h and C-l for going to the previous and to the next
  model in current directory.
* Bug fix: Set initial window size to 1200x700. Fixes appearance on non-tiling
  WMs.
* Bug fix: Set allowed APIs for GL area only if possible (Ubuntu does not have
  this option).

## 0.1 → 0.2

* Add navigation buttons which iterate through all models in the same directory.
* Shadows are now filtered (this makes them to be softer).
* The system `numpy-npy` is updated to version `0.2` and model loading speed is
  increased.
* The light source can now be optionally drawn as a triangle.
* Camera position can be changed by moving the mouse pointer over the drawing
  area with the left button held. The light can be moved similarly while holding
  the right mouse button.

## Version 0.1

* Implement simple voxel viewer. Supported formats: npy
