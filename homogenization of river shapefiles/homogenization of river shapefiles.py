# HOMOGENIZE ARCGIS SHAPEFILES OF RIVERS AND CORRESPONDING TABLES OF BIOPHYSICAL VARIABLES ACROSS BAVARIA FROM 90's and 2000's
# THE RIVER SHAPEFILES REPRESENT THE SAME RIVERS, BUT ARE SEGMENTED DIFFERETNLY.
# THIS SCRIPT REBUILDS THE 90's SEGMENTS ACCORDING TO THE 2000's SEGMENTS
# AND RECALCULATES THE CORRESPONDING TABLE OFF BIOPHYSICAL VARIABLES WITH WEIGHTED AVERAGES.

import arcpy
from arcpy import env
import os
env.overwriteOutput = True

arcpy.env.workspace = "G:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2009.gdb"
outputworkspace = "G:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2009.gdb"

gsk = "G:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2009\GSKnOWK2009.gdb\gewaesserstruktur_uev"
owk = "G:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2009\GSKnOWK2009.gdb\owk"


###
#0.1 Copy overlay feature to output geodatabase before modifying it
# arcpy.CopyFeatures_management(gsk, os.path.join(outputworkspace, "gewaesserstruktur_uev"))

###
#0.2 Copy OWK feature to output geodatabase before modifying it
# arcpy.CopyFeatures_management(owk, os.path.join(outputworkspace, "FWK_BP2009"))

###
#0.5. Change workspace to output geodatabase to continue all further work within this
# arcpy.env.workspace = "I:\ArcGIS\Gewaesserstrukturkartierung\GewaesserstrukturkartierungOWKEZGs.gdb"

###process only###
#1. Snap OWK and overlay featureclass
arcpy.Snap_edit(gsk, [[owk, "EDGE", "900 Meters"]])

arcpy.FeatureClassToFeatureClass_conversion (owk,"in_memory","owk")

#2. Feature Vertices To Points - Feature-Stuetzpunkte in Punkte - OWK END VERTICES
arcpy.FeatureVerticesToPoints_management("in_memory/owk", "in_memory/owk_punkte", "BOTH_ENDS")
# arcpy.FeatureClassToFeatureClass_conversion("in_memory/owk_punkte", outputworkspace, "owk_punkte2")
# because first one wasnt fucking working
arcpy.FeatureClassToFeatureClass_conversion("in_memory/owk_punkte", "C:\Users\Yan\Documents\ArcGIS\Default.gdb", "owk_punkte2")

#*****
#save to memory to avoid error when using add field tool
#arcpy.FeatureClassToFeatureClass_conversion ("Altmuehl_OWK","in_memory","Altmuehl_OWK1")

#3.1 Add OWK field: length - input shape_length values one-to-one - this is so calculations can work with them later on
# - using the original shapefiles length feature does not work.
arcpy.AddField_management("in_memory/owk", "length", "FLOAT")
arcpy.CalculateField_management ("in_memory/owk", "length", "!shape.length!", "PYTHON")

arcpy.FeatureClassToFeatureClass_conversion("in_memory/owk", outputworkspace, "owk")

#4. Add OWK field: total average - average of part-line weighted averages
# arcpy.AddField_management("in_memory/fische2", "weightedaverage", "FLOAT")

###
# Load main line and overlay line into memory for further processing
arcpy.FeatureClassToFeatureClass_conversion (gsk, "in_memory", "gsk")


#3.2. Add overlay line field: total_length - input shape_length values one-to-one - this is so calculations can work with them later on
# - using the original shapefiles length feature does not work.
# add field for total line-part length before spliting overlay line.
arcpy.AddField_management(r"in_memory\gsk", "total_length", "FLOAT")
arcpy.CalculateField_management (r"in_memory\gsk", "total_length", "!shape.length!", "PYTHON")

# arcpy.FeatureClassToFeatureClass_conversion (r"in_memory\gsk", "G:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2009.gdb", "gsk")
# because first one wasnt fucking working
arcpy.FeatureClassToFeatureClass_conversion (r"in_memory\gsk", "C:\Users\Yan\Documents\ArcGIS\Default.gdb", "gsk")

# ONLY IF RELOADING #
arcpy.FeatureClassToFeatureClass_conversion ("G:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2009\GSKnOWK2009.gdb\owk", "in_memory", "owk")
arcpy.FeatureClassToFeatureClass_conversion ("I:\ArcGIS\Gewaesserstrukturkartierung\GSK_BY_Gesamt_20180221.gdb\gsk", "in_memory", "gsk")
#####################

###
#4. Split Line at Point - Linie an Punkt teilen - SPLIT OVERLAY LINE
arcpy.SplitLineAtPoint_management("in_memory/gsk", "in_memory/owk_punkte", "C:\Users\Yan\Documents\ArcGIS\Default.gdb\gsk_split")

# DONE IN ARCMAP WITH EXPORT TO GDB SO FOLLOWING STEP IS NOT NECESSARY
arcpy.FeatureClassToFeatureClass_conversion("in_memory/gsk_uev_Gesamtbewertung_Split", outputworkspace, "gsk_uev_Gesamtbewertung_Split")

###
arcpy.FeatureClassToFeatureClass_conversion ("C:\Users\Yan\Documents\ArcGIS\Default.gdb\gsk_split", "in_memory" ,"gsk_split")

###
#5. Add overlay field: part length - for (desired attribute value * part length) / (original full line length)
# arcpy.AddField_management("in_memory/referenzzoenose_split2", "weighted_value", "FLOAT")
# USE PREVIOUS STEP TO RELOAD GSK_SPLIT TO MEMORY AGAIN IF FOLLOWING CALCULATION WERE CARRIED OUT IN ARCMAP
arcpy.AddField_management("in_memory\gsk_split", "length", "FLOAT")
arcpy.CalculateField_management (r"in_memory\gsk_split", "length", "!shape.length!", "PYTHON")

#*****
#Output referenzzoenose_split2 to HDD for checking
# arcpy.FeatureClassToFeatureClass_conversion (r"in_memory\gsk_uev_Gesamtbewertung_Split2", outputworkspace, "gsk_uev_Gesamtbewertung_Split2")

# 6. RECALCULATE THE TABLE OFF BIOPHYSICAL VARIABLES WITH WEIGHTED AVERAGES.
# Multiply all desired attribute fields (eg. bachforell in referenzzoenose_split2) with shape length to create weighted value
#arcpy.CalculateField_management ("in_memory/referenzzoenose_split2", "weighted_value", "!shape.length!" * "!bachforell!", "PYTHON")
#*****

###
layer = "in_memory\gsk_split"
multfield = "length" # the length of the overlay line-parts resulting from dividing the overlay-line with the endpoints of the main line-parts. This is used to multiply the desired value.
divfield = "total_length" # the length of the original overlay line lengths before dividing it by the end points of the main line parts. This value is used to divide the weighted value from the previous multiplication to arrive at a weighted average.

# create array of fieldnames in feature class
fieldnames = [i.name for i in arcpy.ListFields(layer) if not i.required]

# replace all NULL (blanks) with zeros
# save index of field you want to multiply the remaining fields with (to avoid multiplying by itself later on or just for easier access)
ind = fieldnames.index(multfield)
inddiv = fieldnames.index(divfield)
indexlist = [8, 10, 12, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47]

# use fieldname array to prepare iteration with a selection cursor - using a selection cursor is necessary so that the single rows in each field can be iterated through
cursor = arcpy.da.UpdateCursor(layer, fieldnames)

with arcpy.da.UpdateCursor(layer, fieldnames) as cursor:
# same as "cursor = arcpy.da.UpdateCursor(layer, fieldnames)"
# iterate through fields with selection cursor
    for row in cursor:
        for index in indexlist:
            if row[index] == None:
                row[index] = 0
            cursor.updateRow(row)
        for index in indexlist:
            row[index] = float(row[index])
            cursor.updateRow(row)
        for index in indexlist:
            row[index] = row[index]*row[ind]
            row[index] = row[index]/row[inddiv]
            cursor.updateRow(row)

arcpy.FeatureClassToFeatureClass_conversion (layer, "C:\Users\Yan\Documents\ArcGIS\Default.gdb", "gsk_split_2")

outputworkspace = "I:\ArcGIS\Gewaesserstrukturkartierung\GSKnOWK2014"
arcpy.TableToTable_conversion("I:\ArcGIS\Gewaesserstrukturkartierung\GSK_BY_Gesamt_20180221.gdb\gsk_split_2", outputworkspace, "GSK_all_EZGs_2014")
#    print("reworked file " + file + " exported to txt")

arcpy.FeatureClassToFeatureClass_conversion ("C:\Users\Yan\Documents\ArcGIS\Default.gdb\gsk_split", "in_memory", "gsk_split_2")


#7. Feature Vertices To Points - Feature-Stuetzpunkte in Punkte - REFERENZZOENOSE END VERTICES
arcpy.FeatureVerticesToPoints_management("in_memory\gsk_split_2", "in_memory\gsk_plit_2_punkte", "BOTH_ENDS")

arcpy.FeatureClassToFeatureClass_conversion ("in_memory\gsk_plit_2_punkte", "C:\Users\Yan\Documents\ArcGIS\Default.gdb", "gsk_plit_2_punkte")

#8. Split Line at Point - Linie an Punkt teilen - SPLIT OVERLAY LINE
arcpy.SplitLineAtPoint_management ("in_memory/owk", "in_memory\gsk_plit_2_punkte", "in_memory\owk_split_nach_gsk", "150 Meters")

#0.5. Change workspace to output geodatabase to continue all further work within this
arcpy.env.workspace = r"C:\Users\ga75yam\Dokumente\ArcGIS\Lines\Bodensee.gdb"
outputworkspace = r"C:\Users\ga75yam\Dokumente\ArcGIS\Lines\Bodensee.gdb"
arcpy.FeatureClassToFeatureClass_conversion("in_memory\owk_split_nach_gsk", "C:\Users\Yan\Documents\ArcGIS\Default.gdb", "owk_split_nach_gsk")

#9 Spatial join between
arcpy.FeatureClassToFeatureClass_conversion ("Bodensee_split_nach_gsk_uev_Gesamtbewertung", "in_memory" ,"Bodensee_OWK_split_nach_gsk_uev_Gesamtbewertung")
arcpy.FeatureClassToFeatureClass_conversion ("gsk_uev_Gesamtbewertung_Split3", "in_memory" ,"gsk_uev_Gesamtbewertung_Split3")
arcpy.SpatialJoin_analysis("in_memory\owk_split_nach_gsk", "in_memory\gsk_split_2", "in_memory\owk_gsk_joined", "JOIN_ONE_TO_MANY", "KEEP_COMMON", "", "HAVE_THEIR_CENTER_IN", "50 Meters")
arcpy.FeatureClassToFeatureClass_conversion ("in_memory\owk_gsk_joined", "C:\Users\Yan\Documents\ArcGIS\Default.gdb", "owk_gsk_joined")

# outputworkspace = r"C:\Users\ga75yam\Dokumente\ArcGIS\Lines\Bodensee"
arcpy.TableToTable_conversion("in_memory\owk_gsk_joined", "C:\Users\Yan\Documents\ArcGIS", "owk_gsk_joined.txt")



