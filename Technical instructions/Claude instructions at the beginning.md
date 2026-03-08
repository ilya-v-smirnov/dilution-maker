# Technical Specification: Shiny Application for Laboratory Dilution and Solution Preparation

This file contains base ideas about application structure and functionality prior its actual implementation.

## 1. Core Idea

### Purpose

The application is designed to calculate laboratory dilutions and
solution preparations for biological laboratories.\
Its goal is to transform input parameters into **clear, step‑by‑step
laboratory instructions** suitable for immediate use at the bench.

The application solves not only the mathematical calculation problem but
also the **experimental planning problem**.

The primary output of the system is a **structured laboratory
instruction**, not simply a numeric result.

### Target Users

-   Molecular biology researchers
-   Immunology researchers
-   Biological laboratory staff
-   Scientists working with proteins, antibodies, salts, small molecules
    and buffers

### Core Design Principle

The application is **scenario‑based**, not formula‑based.

Each scenario has: - its own calculation logic - its own UI workflow -
its own memory slot - its own Excel export format - its own export file
name

------------------------------------------------------------------------

# 2. Supported Laboratory Scenarios

## 2.1 Single‑Step Dilution

Used for standard stock → target dilution.

Inputs: - Stock concentration - Target concentration - Final volume -
Diluent

Outputs: - dilution factor - volume of stock - volume of diluent -
laboratory instruction

Example:

58.8 µl antibody + 10 ml PBS | DF: 170x

------------------------------------------------------------------------

## 2.2 Multi‑Step Dilution Planner

Automatically splits dilution if DF is too large.

Priority of the planner: **avoid very large DF per step**.

Planner considers: - DF per step - pipetting limits - intermediate
volumes - warnings for extremely small volumes

Example:

Step 1\
1.00 µl stock + 399 µl PBS | DF: 400x

Step 2\
25.0 µl intermediate + 10.6 ml PBS | DF: 425x

Very small volumes are allowed but warnings are displayed.

------------------------------------------------------------------------

## 2.3 MW‑Based Concentration Conversion

Central scenario used when mass ↔ molarity conversion is required.

Typical examples: - antibody mg/ml → nM - protein µg/ml → µM - small
molecule mM → mg/ml

The calculation uses: - MW database - custom MW entries - manual MW
input

------------------------------------------------------------------------

## 2.4 Prepare Solution from Dry Substance

Used to prepare solutions from powder.

Inputs: - substance - MW - target concentration - final volume - solvent

Output: required mass and preparation instruction.

Example:

100 mM NaCl:\
58.4 mg NaCl + 10 ml water.

------------------------------------------------------------------------

## 2.5 Session Memory

The application supports sequential work.

Workflow:

1 calculation → saved\
next calculation → saved\
results accumulate in memory

All stored results can later be exported.

------------------------------------------------------------------------

# 3. Supported Units

### Mass concentrations

pg/ml\
ng/ml\
µg/ml\
mg/ml

### Molar concentrations

M\
mM\
µM\
nM\
pM

### Volumes

µl\
ml\
L

------------------------------------------------------------------------

# 4. Internal Calculation Units

All values are converted internally to:

Mass concentration → g/L\
Molar concentration → mol/L\
Volume → L

This ensures consistency of calculations.

------------------------------------------------------------------------

# 5. Dilution Approximation Rule

If dilution factor \> 100:

the added stock volume may be ignored when calculating final volume.

Only **one active calculation mode** is displayed in the interface.

------------------------------------------------------------------------

# 6. Rounding Rules

Internal calculations use maximum precision.

Displayed instructions use **3 significant figures**.

------------------------------------------------------------------------

# 7. Interface Structure

Main Shiny tabs:

1.  Single Dilution\
2.  Multi‑Step Planner\
3.  MW‑Based Preparation\
4.  Prepare from Dry Substance\
5.  Saved Recipes\
6.  MW Database\
7.  Export Center

------------------------------------------------------------------------

# 8. Single Dilution Tab

Inputs: - Solution name (optional) - Stock concentration - Target
concentration - Final volume - Diluent - Approximation mode

Outputs: - dilution factor - stock volume - diluent volume - instruction
text - warnings

------------------------------------------------------------------------

# 9. Multi‑Step Planner Tab

Inputs: - stock concentration - target concentration - final volume -
diluent - max DF per step - minimum pipetting volume - preferred
intermediate volume

Outputs: - number of steps - intermediate concentrations - step
instructions - warnings

Manual editing of steps is not included in the first version.

------------------------------------------------------------------------

# 10. MW‑Based Preparation Tab

Inputs: - substance - MW source (database / manual) - stock
concentration - target concentration - final volume

Outputs: - converted concentration - dilution instructions

------------------------------------------------------------------------

# 11. Dry Substance Preparation Tab

Inputs: - substance - MW - target concentration - final volume - solvent

Outputs: required mass and preparation instruction.

------------------------------------------------------------------------

# 12. Saved Recipes Tab

Displays saved results grouped by scenario:

-   Single Dilution
-   Multi‑Step Dilution
-   MW‑Based Preparation
-   Dry Substance Preparation

Each entry displays the **full instruction text**.

------------------------------------------------------------------------

# 13. MW Database

Excel file:

data/molecular_weights.xlsx

Sheets:

common_substances\
custom_substances

Columns:

substance\
MW

Future versions may allow editing inside the app.

------------------------------------------------------------------------

# 14. Application Architecture

Project structure:

app.R

R/ - dilution_calculations.R - unit_conversion.R -
molarity_calculations.R - dilution_planner.R - validation.R -
formatting.R - mw_database.R - memory_manager.R - export_word.R -
export_excel.R

data/ - molecular_weights.xlsx

------------------------------------------------------------------------

# 15. Result Object Structure

Each calculation returns a structured object containing:

scenario_type\
solution_name\
instruction_text\
input_parameters\
normalized_parameters\
steps\
warnings\
technical_metadata

This object is used for: UI display\
memory storage\
Word export\
Excel export

------------------------------------------------------------------------

# 16. Word Export

Single document containing **all stored instructions**.

Structure:

Scenario sections: - Single Dilution - Multi‑Step Dilution - MW‑Based
Preparation - Dry Substance Preparation

Content example:

10 nM antibody in PBS\
58.8 µl stock + 10.0 ml PBS \| DF: 170x

------------------------------------------------------------------------

# 17. Excel Export

Scenario‑specific files.

Examples:

single_dilution_instructions.xlsx\
multistep_dilution_plan.xlsx\
mw_based_preparation.xlsx\
dry_substance_preparation.xlsx

Excel includes additional metadata including MW source.

------------------------------------------------------------------------

# 18. Error Prevention

The application detects:

-   incompatible units
-   missing MW
-   impossible dilution
-   extremely small pipetting volumes
-   inconsistent final volumes

Small volumes trigger warnings but are allowed.

------------------------------------------------------------------------

# 19. Implementation Priorities

## First version

-   scenario architecture
-   single dilution
-   multi‑step planner
-   MW‑based preparation
-   dry substance preparation
-   MW Excel database
-   session memory
-   Word export
-   scenario‑specific Excel export

## Future versions

-   manual editing of dilution plans
-   full database editing
-   additional experimental scenarios

------------------------------------------------------------------------

# 20. Final Concept

The application functions as a **laboratory instruction generator**, not
just a concentration calculator.

It provides: - automated dilution planning - MW‑based calculations -
session memory - reproducible documentation - minimal‑error laboratory
workflows
