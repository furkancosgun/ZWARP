# ZWARP - ABAP Object Transport Helper

## Overview
ZWARP is an ABAP utility that helps identify and transport development objects used within a program or function module. It scans for dependencies and related objects, then allows you to easily add them to a transport request.

## Key Features

- **Object Discovery**: Automatically finds all related objects for a given program/function
- **Custom Object Filter**: Focuses only on custom (Z/Y) objects
- **Transport Integration**: Directly adds selected objects to transport requests
- **ALV Interface**: User-friendly selection and display interface

## Supported Object Types

The program currently supports the following object types:

| Object Class | Object Type | Description               |
|--------------|-------------|---------------------------|
| R3TR         | CLAS        | Class                     |
| R3TR         | DDLS        | Data Definition           |
| R3TR         | DOMA        | Domain                    |
| R3TR         | DTEL        | Data Element              |
| LIMU         | DYNP        | Dynpro                    |
| R3TR         | ENHO        | Enhancement Implementation|
| R3TR         | FUGR        | Function Group            |
| LIMU         | FUNC        | Function Module           |
| R3TR         | INTF        | Interface                 |
| R3TR         | MSAG        | Message Class             |
| R3TR         | PARA        | Parameter                 |
| R3TR         | PROG        | Program                   |
| LIMU         | REPS        | Include                   |
| R3TR         | SHLP        | Search Help               |
| R3TR         | TABL        | Table                     |
| R3TR         | TTYP        | Table Type                |
| R3TR         | VIEW        | View                      |
| LIMU         | STRU        | Structure (converted to TABL) |
| R3TR         | ENQU        | Lock Object               |

## How to Use

1. **Selection Screen**:
   - Enter object types (e.g., PROG for programs)
   - Enter object names (e.g., your program name)

2. **ALV Display**:
   - Check the objects you want to transport
   - Use toolbar buttons:
     - Select All/Deselect All
     - Transport Selected

3. **Transport**:
   - The system will prompt for a transport request
   - Selected objects will be added to the request

## Technical Details

- Uses `REPOSITORY_ENVIRONMENT_ALL` to find related objects
- Processes workbench objects via `WB_ANYTYPE_RETURN_OBJECT_LIST`
- Implements progress indicator during processing
- Custom object detection (Z/Y prefixes)

## Requirements

- SAP system with ABAP development authorization
- Transport organizer access

## Known Limitations

- Only processes custom (Z/Y) objects by default
- Limited to the object types listed above
- Large object sets may take time to process
