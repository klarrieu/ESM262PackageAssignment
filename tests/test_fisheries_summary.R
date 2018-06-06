library(devtools)
library(testthat)
library(ESM262PackageAssignment)

test_that("fisheries_summary_works",
          {
            # generate sample data
            # possible fish species to sample from
            species = c("sockeye","chinook","sculpin","jellyfish","flounder")
            # all prices set to zero for each possible fish
            prices = data.frame(fish = species, price = rep(0, times = 5))
            # locations of fisheries
            locs = c("Earth", "Mars", "Europa", "Titan", "Venus")
            catches = matrix(nrow=length(species), ncol=length(locs) )
            colnames(catches)=locs
            rownames(catches)=species
            # typical size of catch in each area
            locs = data.frame(name=locs, size = c(10000, 100, 20000, 500, 50))
            # generate random catches close to typical sizes for each area
            for ( i in 1: length(species)) {
              for (j in 1:nrow(locs)) {
                catches[i,j] =  round(rnorm(mean=locs$size[j], sd=0.1*locs$size[j], n=1))
              }
            }

            #make sure we get zero revenue if all prices are zero
            expect_that(fisheries_summary(catches = catches, prices = prices)$total_revenue, equals(0))
            expect_that(sum(fisheries_summary(catches = catches, prices = prices)$revenues$revenue), equals(0))
            }

          )
