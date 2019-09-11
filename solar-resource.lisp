#| Examples of usage:
1. Consider the earth to be a blackbody with average surface temperature 15°C 
and area equal to 5.1 x 10^14 m^2. Find the rate at which energy is radiated 
by the earth and the wavelength at which maximum power is radiated. 
Compare this peak wavelength with that for a 5800 K blackbody (the sun).

Rate: (blackbody-total-power-emitted 5.1e14 288) => 2e17 W
Peak wavelength at 288 K: (maximum-power-wavelength 288) => 10.1 um 
Peak wavelength at 5800 K: (maximum-power-wavelength 5800) => 0.45 um

2. "Tilt Angle of a PV Module". Find the optimum tilt angle for
a south-facing photovoltaic module in Tucson (latitude 32.1°) 
at solar noon on March 1.

(pv-module-tilt-angle 
   (solar-noon-altitude-angle 32.1 
                             (solar-declination (day-number 3 1)))) 
=> 40.4°
|#

(defconstant k 5.67e-8
  "Boltzmann constant (in W/m^2/K).")

(defconstant +month-first-day-number+
  '(1 32 60 91 121 152 182 213 244 274 305 335)
  "Day numbers for the first day of each month.")

(defun deg->rad (deg-angle)
  "Convert angle given in degrees into radians."
  (/ (* deg-angle pi) 180))

(defun rad->deg (rad-angle)
  "Convert angle given in degrees into radians."
  (/ (* rad-angle 180) pi))

(defun blackbody-emissive-power (wavelength temperature)
  "Return the blackbody emissive power per unit of area (W/m^2/um),
given the wavelength (um) and the absolute temperature of the blackbody (K).
This is the Planck's law."
  (/ 3.74e8
     (* (expt wavelength 5) (- (exp (/ 14400 (* wavelength temperature)))
			       1))))

(defun blackbody-total-power-emitted (area temperature)
  "Return the total power emitted by the blackbody (W) of the given 
surface area (A) and absolute temperature (K).
This is the Stefan-Boltzmann law."
  (* k area (expt temperature 4)))

(defun maximum-power-wavelength (temperature)
  "Return the wavelength (um) at which the spectrum of the blackbody reaches
its maximum, at the given absolute temperature (K).
This is the Wien's displacement rule."
  (/ 2898.0 temperature))

(defun day-number (month day)
  "Return the day number in the year given the month and the day."
  (+ (nth (1- month) +month-first-day-number+) (1- day)))

(defun distance-earth-sun (day-number)
  "Return the distance (km) between the earth and the sun at the
given day number."
  (* 1.5e8 (+ 1 (* 0.017 (sin (deg->rad (/ (* 360 (- day-number 93))
					   365)))))))
(defun solar-declination (day-number)
  "Return the angle between the sun and the equator line, for the
given day."
  (* 23.45 (sin (deg->rad (* 360/365 (- day-number 81))))))

(defun solar-noon-altitude-angle (latitude declination)
  "Return the angle between the sun and the local horizon at solar noon."
  (+ 90 (- latitude) declination))

(defun pv-module-tilt-angle (altitude-angle)
  "Return the optimum tilt angle for a south-facing at solar noon."
  (- 90 altitude-angle))

(defun solar-altitude-angle (latitude declination hour-angle)
  "Return the angle between the sun and the local horizon at any time."
  (rad->deg (asin (+ (* (cos (deg->rad latitude))
			(cos (deg->rad declination))
			(cos (deg->rad hour-angle)))
		     (* (sin (deg->rad latitude))
			(sin (deg->rad declination)))))))

(defun solar-azimuth-angle (solar-altitude-angle declination hour-angle)
  (rad->deg (/ (* (cos (deg->rad declination))
		  (sin (deg->rad hour-angle)))
	       (cos (deg->rad solar-altitude-angle)))))
