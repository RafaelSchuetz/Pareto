###Erstellen von Einrichtungsdummies, um fixed effects in die Regression einbauen zu können
##Name: dummy103, dummy104...dummy687

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyselect)

# Erstellung eines neuen Datensatzes, der nur die id als Variable enthält

dfcEF_id <- dfcEF[ ,c("id")]

#erstellen der ID-Dummies

dfcEF_id$dummy103 <- ifelse(dfc$id == 103, '1', '0')
dfcEF_id$dummy104 <- ifelse(dfc$id == 104, '1', '0')
dfcEF_id$dummy105 <- ifelse(dfc$id == 105, '1', '0')
dfcEF_id$dummy106 <- ifelse(dfc$id == 106, '1', '0')
dfcEF_id$dummy108 <- ifelse(dfc$id == 108, '1', '0')
dfcEF_id$dummy109 <- ifelse(dfc$id == 109, '1', '0')
dfcEF_id$dummy111 <- ifelse(dfc$id == 111, '1', '0')
dfcEF_id$dummy112 <- ifelse(dfc$id == 112, '1', '0')
dfcEF_id$dummy113 <- ifelse(dfc$id == 113, '1', '0')
dfcEF_id$dummy114 <- ifelse(dfc$id == 114, '1', '0')
dfcEF_id$dummy118 <- ifelse(dfc$id == 118, '1', '0')
dfcEF_id$dummy122 <- ifelse(dfc$id == 122, '1', '0')
dfcEF_id$dummy123 <- ifelse(dfc$id == 123, '1', '0')
dfcEF_id$dummy124 <- ifelse(dfc$id == 124, '1', '0')
dfcEF_id$dummy125 <- ifelse(dfc$id == 125, '1', '0')
dfcEF_id$dummy130 <- ifelse(dfc$id == 130, '1', '0')
dfcEF_id$dummy131 <- ifelse(dfc$id == 131, '1', '0')
dfcEF_id$dummy132 <- ifelse(dfc$id == 132, '1', '0')
dfcEF_id$dummy133 <- ifelse(dfc$id == 133, '1', '0')
dfcEF_id$dummy136 <- ifelse(dfc$id == 136, '1', '0')
dfcEF_id$dummy137 <- ifelse(dfc$id == 137, '1', '0')
dfcEF_id$dummy139 <- ifelse(dfc$id == 139, '1', '0')
dfcEF_id$dummy141 <- ifelse(dfc$id == 141, '1', '0')
dfcEF_id$dummy142 <- ifelse(dfc$id == 142, '1', '0')
dfcEF_id$dummy165 <- ifelse(dfc$id == 165, '1', '0')
dfcEF_id$dummy186 <- ifelse(dfc$id == 186, '1', '0')
dfcEF_id$dummy187 <- ifelse(dfc$id == 187, '1', '0')
dfcEF_id$dummy188 <- ifelse(dfc$id == 188, '1', '0')
dfcEF_id$dummy189 <- ifelse(dfc$id == 189, '1', '0')
dfcEF_id$dummy190 <- ifelse(dfc$id == 190, '1', '0')
dfcEF_id$dummy191 <- ifelse(dfc$id == 191, '1', '0')
dfcEF_id$dummy192 <- ifelse(dfc$id == 192, '1', '0')
dfcEF_id$dummy193 <- ifelse(dfc$id == 193, '1', '0')
dfcEF_id$dummy194 <- ifelse(dfc$id == 194, '1', '0')
dfcEF_id$dummy209 <- ifelse(dfc$id == 209, '1', '0')
dfcEF_id$dummy213 <- ifelse(dfc$id == 213, '1', '0')
dfcEF_id$dummy214 <- ifelse(dfc$id == 214, '1', '0')
dfcEF_id$dummy215 <- ifelse(dfc$id == 215, '1', '0')
dfcEF_id$dummy216 <- ifelse(dfc$id == 216, '1', '0')
dfcEF_id$dummy217 <- ifelse(dfc$id == 217, '1', '0')
dfcEF_id$dummy218 <- ifelse(dfc$id == 218, '1', '0')
dfcEF_id$dummy219 <- ifelse(dfc$id == 219, '1', '0')
dfcEF_id$dummy220 <- ifelse(dfc$id == 220, '1', '0')
dfcEF_id$dummy221 <- ifelse(dfc$id == 221, '1', '0')
dfcEF_id$dummy226 <- ifelse(dfc$id == 226, '1', '0')
dfcEF_id$dummy233 <- ifelse(dfc$id == 233, '1', '0')
dfcEF_id$dummy249 <- ifelse(dfc$id == 249, '1', '0')
dfcEF_id$dummy255 <- ifelse(dfc$id == 255, '1', '0')
dfcEF_id$dummy269 <- ifelse(dfc$id == 269, '1', '0')
dfcEF_id$dummy270 <- ifelse(dfc$id == 270, '1', '0')
dfcEF_id$dummy281 <- ifelse(dfc$id == 281, '1', '0')
dfcEF_id$dummy282 <- ifelse(dfc$id == 282, '1', '0')
dfcEF_id$dummy403 <- ifelse(dfc$id == 403, '1', '0')
dfcEF_id$dummy404 <- ifelse(dfc$id == 404, '1', '0')
dfcEF_id$dummy417 <- ifelse(dfc$id == 417, '1', '0')
dfcEF_id$dummy418 <- ifelse(dfc$id == 418, '1', '0')
dfcEF_id$dummy437 <- ifelse(dfc$id == 437, '1', '0')
dfcEF_id$dummy482 <- ifelse(dfc$id == 482, '1', '0')
dfcEF_id$dummy483 <- ifelse(dfc$id == 483, '1', '0')
dfcEF_id$dummy599 <- ifelse(dfc$id == 599, '1', '0')
dfcEF_id$dummy600 <- ifelse(dfc$id == 600, '1', '0')
dfcEF_id$dummy601 <- ifelse(dfc$id == 601, '1', '0')
dfcEF_id$dummy602 <- ifelse(dfc$id == 602, '1', '0')
dfcEF_id$dummy623 <- ifelse(dfc$id == 623, '1', '0')
dfcEF_id$dummy663 <- ifelse(dfc$id == 663, '1', '0')
dfcEF_id$dummy664 <- ifelse(dfc$id == 664, '1', '0')
dfcEF_id$dummy665 <- ifelse(dfc$id == 665, '1', '0')
dfcEF_id$dummy666 <- ifelse(dfc$id == 666, '1', '0')
dfcEF_id$dummy667 <- ifelse(dfc$id == 667, '1', '0')
dfcEF_id$dummy684 <- ifelse(dfc$id == 684, '1', '0')
dfcEF_id$dummy685 <- ifelse(dfc$id == 685, '1', '0')
dfcEF_id$dummy686 <- ifelse(dfc$id == 686, '1', '0')
dfcEF_id$dummy687 <- ifelse(dfc$id == 687, '1', '0')

#73 neue Variablen wurden erstellt, für 73 verschiedene ID´s
#keine ausgelassen, da wir Beta0 weglassen -> keine perfekte multikollinearität

# Problem: Der id-Dummies liegen im Datentyp "character" vor.
# Daher wird der Datentyp aller id-Dummies zu "numeric" umgewandelt.

dfcEF_id <- data.frame(lapply(dfcEF_id, function(x) as.numeric(as.character(x))))

# Hinzufügen der id-Dummies zum ursprünglichen Datensatz dfcEF

dfcEF$dummy103 <- dfcEF_id$dummy103
dfcEF$dummy104 <- dfcEF_id$dummy104
dfcEF$dummy105 <- dfcEF_id$dummy105
dfcEF$dummy106 <- dfcEF_id$dummy106
dfcEF$dummy108 <- dfcEF_id$dummy108
dfcEF$dummy109 <- dfcEF_id$dummy109
dfcEF$dummy111 <- dfcEF_id$dummy111
dfcEF$dummy112 <- dfcEF_id$dummy112
dfcEF$dummy113 <- dfcEF_id$dummy113
dfcEF$dummy114 <- dfcEF_id$dummy114
dfcEF$dummy118 <- dfcEF_id$dummy118
dfcEF$dummy122 <- dfcEF_id$dummy122
dfcEF$dummy123 <- dfcEF_id$dummy123
dfcEF$dummy124 <- dfcEF_id$dummy124
dfcEF$dummy125 <- dfcEF_id$dummy125
dfcEF$dummy130 <- dfcEF_id$dummy130
dfcEF$dummy131 <- dfcEF_id$dummy131
dfcEF$dummy132 <- dfcEF_id$dummy132
dfcEF$dummy133 <- dfcEF_id$dummy133
dfcEF$dummy136 <- dfcEF_id$dummy136
dfcEF$dummy137 <- dfcEF_id$dummy137
dfcEF$dummy139 <- dfcEF_id$dummy139
dfcEF$dummy141 <- dfcEF_id$dummy141
dfcEF$dummy142 <- dfcEF_id$dummy142
dfcEF$dummy165 <- dfcEF_id$dummy165
dfcEF$dummy186 <- dfcEF_id$dummy186
dfcEF$dummy187 <- dfcEF_id$dummy187
dfcEF$dummy188 <- dfcEF_id$dummy188
dfcEF$dummy189 <- dfcEF_id$dummy189
dfcEF$dummy190 <- dfcEF_id$dummy190
dfcEF$dummy191 <- dfcEF_id$dummy191
dfcEF$dummy192 <- dfcEF_id$dummy192
dfcEF$dummy193 <- dfcEF_id$dummy193
dfcEF$dummy194 <- dfcEF_id$dummy194
dfcEF$dummy209 <- dfcEF_id$dummy209
dfcEF$dummy213 <- dfcEF_id$dummy213
dfcEF$dummy214 <- dfcEF_id$dummy214
dfcEF$dummy215 <- dfcEF_id$dummy215
dfcEF$dummy216 <- dfcEF_id$dummy216
dfcEF$dummy217 <- dfcEF_id$dummy217
dfcEF$dummy218 <- dfcEF_id$dummy218
dfcEF$dummy219 <- dfcEF_id$dummy219
dfcEF$dummy220 <- dfcEF_id$dummy220
dfcEF$dummy221 <- dfcEF_id$dummy221
dfcEF$dummy226 <- dfcEF_id$dummy226
dfcEF$dummy233 <- dfcEF_id$dummy233
dfcEF$dummy249 <- dfcEF_id$dummy249
dfcEF$dummy255 <- dfcEF_id$dummy255
dfcEF$dummy269 <- dfcEF_id$dummy269
dfcEF$dummy270 <- dfcEF_id$dummy270
dfcEF$dummy281 <- dfcEF_id$dummy281
dfcEF$dummy282 <- dfcEF_id$dummy282
dfcEF$dummy403 <- dfcEF_id$dummy403
dfcEF$dummy404 <- dfcEF_id$dummy404
dfcEF$dummy417 <- dfcEF_id$dummy417
dfcEF$dummy418 <- dfcEF_id$dummy418
dfcEF$dummy437 <- dfcEF_id$dummy437
dfcEF$dummy482 <- dfcEF_id$dummy482
dfcEF$dummy483 <- dfcEF_id$dummy483
dfcEF$dummy599 <- dfcEF_id$dummy599
dfcEF$dummy600 <- dfcEF_id$dummy600
dfcEF$dummy601 <- dfcEF_id$dummy601
dfcEF$dummy602 <- dfcEF_id$dummy602
dfcEF$dummy623 <- dfcEF_id$dummy623
dfcEF$dummy663 <- dfcEF_id$dummy663
dfcEF$dummy664 <- dfcEF_id$dummy664
dfcEF$dummy665 <- dfcEF_id$dummy665
dfcEF$dummy666 <- dfcEF_id$dummy666
dfcEF$dummy667 <- dfcEF_id$dummy667
dfcEF$dummy684 <- dfcEF_id$dummy684
dfcEF$dummy685 <- dfcEF_id$dummy685
dfcEF$dummy686 <- dfcEF_id$dummy686
dfcEF$dummy687 <- dfcEF_id$dummy687
