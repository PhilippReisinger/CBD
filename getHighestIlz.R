getHighestIlz <- function(ilzen, iartHierarchy = c("VB", "F4", "F3", "F2", "F1", "FH", "KA", "KK", "KI")) {

  # ilzList:          Vektor von ILZen
  # iartHierarchy:    Vektor der Insitutsarten (Hierarchie in absteigender Prio)
  #                   z.B.: c("VB", "F4", "F3", "F2", "F1", "FH", "KA", "KK", "KI")

  # Duplikate bei ilzen eliminieren
  ilzen <- unique(ilzen)

  # BLZ und IART erstellen
  blz <- substr(ilzen,1,5)
  iart <- substr(ilzen,6,7)

  # Sortierung vorbereiten
  iartSort <- 1:length(iartHierarchy)
  names(iartSort) <- iartHierarchy

  # Ueberpruefung, ob iarten in den ilzen enthalten sind, die nicht in der iartHierarchy beruecksichtigt sind und ggf. Ausgabe eines Warnings
  omittedIarts <- ilzen[!(iart %in% iartHierarchy)]

  if (length(omittedIarts) > 0) {
    warning("Folgende IART(en) wird/werden nicht berücksichtigt, da sie in der Hierarchie nicht enthalten ist/sind: ",paste(unique(substr(omittedIarts,6,7)),collapse = " "))
  } else {
    #nix
  }

  # ilzen in iartHierarchy werden berücksichtigt
  ilzen <- ilzen[iart %in% iartHierarchy]

  ### Table für Haeufigkeiten der BLZ-Vorkommen erstellen
  tblBLZ <- table(blz)
  multiBLZ <- names(tblBLZ)[tblBLZ > 1]

  ### BLZen, die nur einmal vorkommen, bleiben erhalten
  ilzen_1 <- ilzen[!blz %in% multiBLZ]

  ### BLZen, die mehrmals vorkommen, werden gemaesz Hierarchie bereinigt (d.h. sortiert und Mehrfachvorkommen geloescht)
  ilzen_2 <- ilzen[blz %in% multiBLZ]
  blzSort <- as.numeric(paste(substr(ilzen_2, 1, 5), iartSort[substr(ilzen_2, 6, 7)], sep = ""))
  ilzen_2 <- ilzen_2[order(blzSort)]
  ilzen_2 <- ilzen_2[!duplicated(substr(ilzen_2, 1, 5))]

  ### ilzen_1 und ilzen_2 zusammenfuehren
  ilzen <- c(ilzen_1, ilzen_2)

  ### NAs entfernen
  #ilzen <- ilzen[!is.na(ilzen)]

  ### nach ILZ sortieren
  ilzen <- ilzen[order(ilzen)]

  ### ILZen rueckgeben
  return(ilzen)

}
