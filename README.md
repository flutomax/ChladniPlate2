# Chladni Plate 2
### Cymatic simulation of Chladni figures program 

[![Download Latest](https://img.shields.io/badge/download-latest-green.svg)](https://github.com/flutomax/ChladniPlate2/releases/)
[![Donate](https://img.shields.io/badge/donate-paypal-blue.svg)](https://paypal.me/flutomax)

Using the **Chladni Plate 2** program, you can model Chladni figures based on a set of waveforms. You can set waveform parameters such as:
1. Amplitude. Specifies the amplitude of the waveform. The range of values is -1.0..1.0.
2. Frequency ratio. Sets the frequency of the wave oscillation. The value means the harmonic of the oscillation. The range of values is 0.1..20.0.
3. Phase. Specifies the phase of the waveform oscillation. Value range -360.0°..360.0°

After rendering, a level map is formed, which you can colorize with any color map by selecting it from the list on the right.
You can also set the image size, the capacity of the list of values, and also check the "Save Levels Map With File" checkbox, which allows you to save the calculated level maps in the working file. These options can be set in the Properties window. 
The program provides the "Normalize" option, which allows you to normalize the resulting signal to the range 0.0..1.0, which fully uses the full range of color maps without limitation.
You can randomize the values of the frequency components by using the corresponding command, which allows you to get fancy figures.

![ScreenShot](/screenshots/cladni_plate2_scr.jpg)

The program is written in the Object Pascal language in the Lazarus IDE, and can be compiled for various operating systems Windows, MacOS, Linux and others. In this repository you can download compiled binaries [here](https://github.com/flutomax/ChladniPlate2/releases/) for these popular operating systems. The package also includes demo examples.



