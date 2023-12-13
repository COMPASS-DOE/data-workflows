# design_table_README.md

The design table, `design_table.csv`, is the central piece of metadata
in the entire COMPASS-FME data processing system, mapping datalogger data variables
to 'design links' that encode the physical location and measurement type of that
variable. It is read and used by `L1_normalize.qmd`.

Design table fields:

| Name               | Description                |
| ------------------ | -------------------------- |
| Site               | Site abbreviation          |
| Logger             | Datalogger name            |
| Table              | Datalogger table name      |
| loggernet_variable | Datalogger variable name   |
| design_link        | Design link (see below)    |
| valid_through      | Expiry date of design link |
| research_name      | Type of measurement        |
| Note               | Note                       |

The table can have empty lines; this can be visually useful, for example to 
separate information about different tables.

Design links follow a pattern of {what}-{site}-{plot}-{which}, e.g.
`GW_BattV-TMP-F-200B` (groundwater battery voltage, TEMPEST, Freshwater plot, the 200B AquaTroll).

The `valid_through` column is used when a sensor is reassigned, for example if a tree
dies and we reassign its sapflux sensor to a new tree, and encodes the last valid
date for a given design link. In this case the loggernet variable has _two_ entries
(rows): the original assignment, with a YYYY-MM-DD `valid_through` entry, and the new assignment,
with a blank `valid_through` entry.

The TEMPEST sapflow assignments are based on
COMPASS -> COMPASS FME -> ARCHIVE _ Pilot Project -> COMPASS FME Task 2 -> TEMPEST -> Sensor Networks & Infrastructure -> Monitoring Documents -> TEMPEST Sap Flow Monitoring

The Synoptic Sap Flow Monitoring, TEROS Monitoring, and Aquatroll Monitoring documents live here:
COMPASS -> COMPASS FME -> ARCHIVE _ Pilot Project -> COMPASS FME Task 1 -> 1.2 Synoptic -> Sensor Maintenance
