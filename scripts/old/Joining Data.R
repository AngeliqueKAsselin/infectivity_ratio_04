# Joining the TCID50 data from all the batches in MA stuff 
batch1 <- read.csv("batch.csv")
batch2 <- read.csv("batch.csv")
joined <-
  full_join(batch1,
            batch2,
            by = c(
              "sample",
              "tcid50",
              "IU_mL",
              "treatment",
              "infectivity_ratio",
              "virus"
            ))
