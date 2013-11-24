
# Example RC Controllers

SMACCMPilot is compatible with a wide variety of RC transmitters and receivers,
as long as they provide 6 or more channels and PPM output. We will describe two
sample RC control systems which we've successfully used with SMACCMPilot.

## FRSKY Taranis

TODO: Taranis image

FRSKY recently released the Taranis, a transmitter which is compatible with
their range of recievers. It has a better build quality than the Turnigy 9XR,
and comes with a rechargable transmitter battery & wall wart charger. As of this
writing (Nov 2013) the Taranis is sold by Aloft Hobbies in the US, but is in
short supply.

### FRSKY Receivers

The Taranis system is offered bundled with the X8R receiver. XXX, lookup how to
put X8R into PPM mode, and check that PPM period is correct.

The [FRSKY D4R-II receiver][d4r-ii] is small, inexpensive, and offers PPM
output. However, the PPM output is only valid when the user limits their use to
only 6 channels. Please be sure to set channels 7 and 8 to output a low pulse
width (1000us) when using the D4R-II module.

## Turnigy 9XR w/ FRSKY radio gear

On our [shopping list][] page we recommend the [Turnigy 9XR
radio][9xr-hobbyking]. The 9XR is one of the least expensive radios which can be
configured with all of the features required for SMACCMPilot. The aesthetics and
build quality leave something to be desired, but it gets the job done.

[shopping list]: shoppinglist.html

![*Image: hobbyking.com*](../images/9xr_hobbyking.jpg)

[9xr-hobbyking]: http://hobbyking.com/hobbyking/store/__31544__Turnigy_9XR_Transmitter_Mode_2_No_Module_.html

The 9XR is designed to take interchangeable radio transmitter modules for
inter-operation with various radio receivers. The simplest way to get a radio
system with the PPM capability we need is with [FRSKY][] radio
modules.

The [FRSKY DJT transmitter module][djt] is compatible with the 9XR transmitter.
The [FRSKY D4R-II receiver][d4r-ii] is small, inexpensive, and offers PPM
output. See caveat about the D4R-II in the FRSKY Taranis section above.

### Mixing for 9x radio

In the 9XR radio, setting up the following the mixer screen will give correct
behavior with the 'THR CUT' switch used for arming and the 'AUX.3' switch used
for mode selection.

![](../images/9x-mixerscreen.jpg)



[FRSKY]: http://www.frsky-rc.com
[djt]: http://www.frsky-rc.com/product/pro.php?pro_id=8
[d4r-ii]: http://www.frsky-rc.com/product/pro.php?pro_id=24

