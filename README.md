Notebook for Hut et al. 2019
================

  - [Data exploration](#data-exploration)
      - [Overview of all sensors and locomotion
        scores](#overview-of-all-sensors-and-locomotion-scores)
      - [Overview of sensors value
        distributions](#overview-of-sensors-value-distributions)
      - [Overview of activity sensors
        values](#overview-of-activity-sensors-values)
      - [Overview of eating sensors
        values](#overview-of-eating-sensors-values)
      - [Overview of BCS data and
        changes](#overview-of-bcs-data-and-changes)
  - [Models](#models)
      - [Activity](#activity)
      - [Eating behaviour](#eating-behaviour)
      - [Rumination behaviour](#rumination-behaviour)
      - [Prediction models including
        BCS](#prediction-models-including-bcs)
      - [Association models BCS vs
        LocomotionScores](#association-models-bcs-vs-locomotionscores)
  - [Remarks and meeting notes](#remarks-and-meeting-notes)
      - [Meeting Mirjam Nielen](#meeting-mirjam-nielen)
      - [Meeting Mirjam, Jan, Miel & Peter
        (23-04-2019)](#meeting-mirjam-jan-miel-peter-23-04-2019)
      - [Meeting Jan](#meeting-jan)
      - [Meeting Mirjam, Jan, Miel, Peter & Klaas
        (27-05-2019)](#meeting-mirjam-jan-miel-peter-klaas-27-05-2019)
      - [Major update with NEW DATA](#major-update-with-new-data)
      - [Figure update](#figure-update)
  - [Review 1](#review-1)
  - [Review 2](#review-2)

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.
\# Getting the data from the bovi-analytics blob storage

You will need to get access key via Miel Hostens (<m.m.hostens@uu.nl>)

The data contains data from 8 farms using NEDAP sensors. An observation
from the NEDAP sensores was collected together with locomotion scores
from the animals.

  - HerdIdentifier - Identifier for the herd
  - Animalidentifier : Identifier for the animal (her eartag)
  - AnimalNumber : Secondary identifier for the animal
  - CalvingTime : Date of calving
  - SensorDaysInMilk : Days in milk at sensor observation
  - SensorDate : Date at sensor observation
  - SensorType: Multiple sensor observations were obtained, the type
    identifies which ones are usefull
  - SensorValue: Sensor value
  - ObservationDate: Date at which the locomotion or bcs scores were
    observed
  - ScoreDaysInMilk : Days in milk at which the locomotion or bcs scores
    were observed
  - ScoreType : Locomotion or BCS score
  - ScoreValue : Score value
  - ObservationDays : Days from sensor to score observation moment

# Data exploration

The underlying images can be seen in the [Tableau
workbook](Visuals/SenseOfSensorsv2.twb):

  - Make sure to have the data downloaded on disk
  - Make sure to have the data referenced correctly in the workbook

## Overview of all sensors and locomotion scores

![Sensor overview](Figures/SensorObservationOverview.png)

## Overview of sensors value distributions

![Distribution overview](Figures/DistributionOverview.png)

## Overview of activity sensors values

![Activity overview](Figures/ActivitySensorOverview.png)

## Overview of eating sensors values

![Eating overview](Figures/RuminationSensorOverview.png)

## Overview of BCS data and changes

![BCS overview](Figures/BCS%20Dashboard.png)

# Models

## Activity

  - [Link to model Standups](StandUps/StandUps.md)
  - [Link to model Leg Activity](LegActivity/LegActivity.md)
  - [Link to model Lying Time](LyingTime/LyingTime.md)
  - [Link to model Lying Bouts](LyingBouts/LyingBouts.md)
  - [Link to model Lying Bout
    Length](LyingBoutLength/LyingBoutLength.md)

## Eating behaviour

  - [Link to model Eating Time](EatingTime/EatingTime.md)
  - [Link to model Eating Bouts](EatingBouts/EatingBouts.md)
  - [Link to model Eating Bout
    Length](EatingBoutLength/EatingBoutLength.md)

## Rumination behaviour

  - [Link to model Rumination
    Time](RuminationTime/PredictionLocoWeek4.md)
  - [Link to model Rumination
    Bouts](RuminationBouts/PredictionLocoWeek8.md)
  - [Link to model Rumination Bout
    Length](RuminationBoutLength/PredictionLocoChange.md)

## Prediction models including BCS

  - [Link to model Week 4 locomotion
    prediction](PredictionModels/RuminationTime.md)
  - [Link to model Week 8 locomotion
    prediction](PredictionModels/RuminationBouts.md)
  - [Link to model AllWeeks locomotion
    prediction](PredictionModels/RuminationBoutLength.md)

## Association models BCS vs LocomotionScores

  - [Link to model Week 4
    locomotion](AssociationModels/AssociationLocoWeek4.md)
  - [Link to model Week 8
    locomotion](AssociationModels/AssociationLocoWeek8.md)

# Remarks and meeting notes

## Meeting Mirjam Nielen

  - error bar -\> CI DONE
  - \-1 tot +1 DONE
  - referentie tijdstip naar 4w DONE
  - lying bouts contains DataType DONE

## Meeting Mirjam, Jan, Miel & Peter (23-04-2019)

  - Add fixed effect pre and post partum DONE
  - heifers separate analysis –\> parity 2, 3 and 4+ depending on group
    size DONE
  - grouping loco scores: 1&2, 3, 4&5 DONE
  - loco score 3 as reference, 1&2 as reference as well. check both for
    contrast (relevel) DONE
  - QQ plot log link DONE
  - partum x ls x moment DONE BUT UNABLE TO FIT
  - types of behavior for analysis: eating time/day, rumination
    time/day, eating+rumination time per day, lying time per day, number
    of steps per day (legactivity)
  - discussion: correlation between behavior of residuals in neck & leg
    sensor data output
  - discussion: visualisation
  - after definitive results: involve Klaas Frankena

## Meeting Jan

  - Wald -\> profile likelihood
  - Force model to be fact1\*fact2 -\> fact1:fact2 + fact2 (this is
    factor one nested within factor 2)

## Meeting Mirjam, Jan, Miel, Peter & Klaas (27-05-2019)

  - Short values of lyingboutlength can be the cutoff on 24h DONE
  - cutoff to 1440 DONE
  - Use log of lyingboutlength DONE
  - LyingBout only -1 ??? - if no diff leave, if yes diff keep that
    model DONE (See [Lying Bouts folder](LyingBouts/)
  - At model scale, a difference was detected … is good way of writing
    the article
  - When comparing models, keep ALL main effects in, when CONFINT then
    drop DONE

## Major update with NEW DATA

  - Request lying bout and lying bout length

## Figure update

  - Check LSM and Figure LSM output (DONE)
  - Check CI instead of SE (DONE)
  - Percentage calculations tableau

# Review 1

  - Changed trend symbol in figure
  - Increase from -1 and 1 to -2 and 2 window around scoring
  - Added need for 4 sensorvalues per average
  - Add dplyr for distinct numbers

# Review 2

  - Added calving season
