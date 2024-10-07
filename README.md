# Frontier League Arm Angles

This repo works to infer arm angles for Frontier League pitchers using their height, release height, and release side using Yakkertech data. The [original analysis](https://github.com/trevorwthrash/armAngles) was done in python by Github user [Trevor Thrash](https://github.com/trevorwthrash/armAngles). This is my attempt at recreating his methodology in R and creating different plots in ggplot and plotly. 

## Example Plots

### Cole Cook Movement Plot
![Cole Cook Movement Plot](example_plots/Cole_Cook_Movement.jpg)

### Cole Cook Savant Plot
![Cole Cook Savant Plot](example_plots/Cole_Cook_Savant.jpg)

### Cole Cook Arm Angle Plot
![Cole Cook Arm Angle Plot](example_plots/Cole_Cook_Arm_Angle.jpg)





One issue is the reliability and accuracy of Yakkertech data. There are a lot of pitches where release heights are over 8 feet (up to 50 feet) and below 1 foot (fdr non submarine pitchers. Relesase sides and extension also have these inaccuracies. 
