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
| valid_through        | Expiry date of design link |
| research_name      | Type of measurement        |
| Note               | Note                       |

The table can have empty lines; this can be visually useful, for example to 
separate information about different tables.

Design links tend to follow a pattern of {sensor}-{datum}-{site}-{plot}, e.g.
`GW-Salinity-PTR-UP` (groundwater, salinity, Portage River, upland). The format
is currently not enforced or consistent, however.

The `valid_through` column is used when a sensor is reassigned, for example if a tree
dies and we reassign its sapflux sensor to a new tree, and encodes the last valid
date for a given design link. In this case the loggernet variable has _two_ entries
(rows): the original assignment, with a YYYY-MM-DD `valid_through` entry, and the new assignment,
with a blank `valid_through` entry.

The TEMPEST TEROS assignments are based on 
https://docs.google.com/spreadsheets/d/1IFHNaE4Tr45rjDNR7MBg7i2Z2ewIPyAhl6q2vCB_Tbw/edit#gid=0
as of 2023-12-11.

