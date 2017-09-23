# VagrantGUI
**This project is a work in progress. Please don't use it yet.**

## Goal
Create a simple cross-platform GUI to Vagrant, that:

- lists all Vagrant boxes
- starts, stops, suspends and resumes boxes

## Comment
The GUI is basically working but is not very responsive. Time-taking commands should be executed as a thread or with the help of `TProcess` and a callback method (simpler and preferred.)
It will be my first attempt to get the program working on all three major platforms (Linux, Windows, MacOS).

