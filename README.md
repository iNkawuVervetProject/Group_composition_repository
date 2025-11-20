# Group_composition_repository
This repository contains basic scripts to calculate group composition and individual presence in groups using the demographic data collected at IVP. Group composition can be used to calculate reliable group size over time, but also adult sex ratio's, number of adult males, females or juveniles.

Group composition is calculated from presence matrices, which are based on birth, death and immigration dates of individuals recorded in the life history file. Since not all dates are fully correct in this life history file, presence is then updated based on the last dates an individual is recorded to have had an interaction. In this script, the interactions used are agonistic interactions, but in theory you could use any behavioural file. Updated presence is not necessary but optional, and only improves your precision with one or two days, which is not always important.

To calculate group composition, you need the following data:
  1. Life history file
  2. Agonistic file (optional)

Before calculating group composition, you need to create:
  1. Presence/absence matrices for all applicable groups (using life history)
  2. Updated presence files using agonistic or other behaviour file (optional)
