# Data explanation

The two files are for submissions (`BMJSubmissions.RData`) and reviews (`BMJReviewers.RData`).

The variables are:
* `local.date`, the date of the submission or review in the local time zone
* `local.hour`, the hour of the submission or review in the local time zone (0 to 24)
* `window`, the weekly window
* `country`, the country
* `journal`, the journal (BMJ or BMJ Open)
* `timezoneId`, the time zone
* `weekend`, if the submission or review was on the weekend
* `late.night`, if the submission or review was during the early morning or late night
* `holiday`, if the submission or review was on a national holiday
