# Flight Checklists

## Preflight Checklist

1. Power on RC transmitter, set channel 5 (mode) to stabilize position, channel
   6 (safety) to disarmed position.
2. Power on vehicle, keeping hands clear of propellers. Keep vehicle still while
   blue LED on FMU is blinking - this indicates that the inertial sensors are
   calibrating.
3. Check that ground control station 3DR Radio has a solid green light
   indicating a successful link.
4. Start ground control station gateway & MAVProxy using the `gcs.sh` script.
5. MAVProxy will indicate a successful link and complete fetching parameters.
6. Use MAVProxy to load parameters for your vehicle. For the 3DR Quad, you can
   load the default parameters with `param load quad3dr_params.txt`.
7. If you will be using the gamepad to control flight, make sure it is plugged
   into the GCS computer, and run the `joystick` command in MAVProxy to enable
   control. The gamepad should be used by a separate pilot.
8. Stand clear of vehicle. Make sure RC transmitter channel 5 (mode) is still
   in the stabilize position. Set RC transmitter channel 6 to the armed
   position.  Move the throttle stick down and right to complete the arming
   sequence.

## Flight Control

1. Move the thottle stick upwards, and take off.
2. Reach a stable altitude of at least 1m above the ground. Change the mode
   switch to altitude hold mode. Set the throttle stick on the RC transmitter to
   50% throttle. The vehicle will hold altitude.
3. Move the throttle stick to about 75% throttle to slowly climb. Return to
   center to hold altitude. Test the same behavior for slowly descending with a
   throttle of about 25%.
4. When the gamepad pilot is ready to take control, move the RC transmitter mode
   switch to auto mode. The gamepad pilot can then hold the gamepad deadman
   switch, and use the gamepad joysticks to control the vehicle.
5. To take control back from the gamepad to the RC transmitter, move the mode
   switch to altitude hold mode.

