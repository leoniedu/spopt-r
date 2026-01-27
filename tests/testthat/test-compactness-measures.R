library(sf)
library(testthat)

# Define a single simple polygon for testing ----

shape <- sf::st_sfc(
  sf::st_polygon(
    list(
      matrix(
        c(
          0, 0,
          0.25, 0.25,
          0, 0.5,
          0.25, 0.75,
          0, 1,
          1.25, 1,
          0.75, 0.5,
          1.25, 0,
          0, 0
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )
)

ATOL <- 0.001

# Create test geometries ----
# Test examples are adapted from the PySAL 'esda' library: "esda/tests/test_shape.py"

test_geom_translated <- sf::st_as_sfc(
  "POLYGON ((-3.1823503126754247 0.085191513232644, -3.2545854200972997 0.271135116748269, -3.2472001661910497 0.296769882373269, -3.2779008497847997 0.3333146821779565, -3.286461030448862 0.4668824922365502, -3.312919770683237 0.4887788545412377, -3.308738862480112 0.5528352510256127, -3.271751557792612 0.6005037080568627, -3.2749711622847997 0.6791780244631127, -3.301475678886362 0.7266938936037377, -3.3279496779097997 0.7266252290529565, -3.3439103712691747 0.8217256318849877, -3.385307465995737 0.9057634126467065, -3.385490571464487 1.0868776094240502, -3.4014665236129247 1.1563432466310815, -3.3219682325972997 1.1584108125490502, -3.3168107618941747 1.328538681201394, -3.2181779493941747 1.3360688936037377, -3.2150346388472997 1.4000871431154565, -3.1089555372847997 1.4057023774904565, -3.105888520683237 1.9232576143068627, -2.702140962089487 1.9063203584474877, -2.7046891798629247 0.8271501313967065, -2.749656831230112 0.7665956270021752, -2.671684419120737 0.5808656465334252, -2.8658219923629247 0.3313920747560815, -2.9108354200972997 0.1093156587404565, -3.1823503126754247 0.085191513232644))"
)[[1]]

# test_simple: difference of two boxes
test_simple <- sf::st_difference(
  sf::st_polygon(list(matrix(c(-1, 0, -2, 0, -2, 1, -1, 1, -1, 0), ncol = 2, byrow = TRUE))),
  sf::st_polygon(list(matrix(c(-1, 0, -1.5, 0, -1.5, 0.5, -1, 0.5, -1, 0), ncol = 2, byrow = TRUE)))
)

# test_hole: box with a hole
test_hole <- sf::st_difference(
  sf::st_polygon(list(matrix(c(0, 0, 1.81, 0, 1.81, 1.81, 0, 1.81, 0, 0), ncol = 2, byrow = TRUE))),
  sf::st_polygon(list(matrix(c(0.8, 0.8, 1.6, 0.8, 1.6, 1.6, 0.8, 1.6, 0.8, 0.8), ncol = 2, byrow = TRUE)))
)

# test_mp: multipolygon (union of two boxes)
test_mp <- st_union(
  sf::st_polygon(list(matrix(c(-1, -1, -1.5, -1, -1.5, -2, -1, -2, -1, -1), ncol = 2, byrow = TRUE))),
  sf::st_polygon(list(matrix(c(0, -1, 1.25, -1, 1.25, -2, 0, -2, 0, -1), ncol = 2, byrow = TRUE)))
)

# test_mp_hole: multipolygon with holes (union of two transformed test_hole polygons)
# Transform 1: -x + 3, y * 0.5 + 3
test_hole_coords <- sf::st_coordinates(test_hole)[, c("X", "Y")]
transformed_1_coords <- cbind(-test_hole_coords[, "X"] + 3, test_hole_coords[, "Y"] * 0.5 + 3)

# Need to reconstruct the polygon with hole structure
# Get the ring structure from test_hole
test_hole_matrix <- sf::st_coordinates(test_hole)
ring_ids <- unique(test_hole_matrix[, "L1"])

# Build transformed polygon 1
rings_1 <- lapply(ring_ids, function(rid) {
  ring_coords <- test_hole_matrix[test_hole_matrix[, "L1"] == rid, c("X", "Y")]
  cbind(-ring_coords[, "X"] + 3, ring_coords[, "Y"] * 0.5 + 3)
})
transformed_1 <- sf::st_polygon(rings_1)

# Transform 2: x + 4, y (unchanged)
rings_2 <- lapply(ring_ids, function(rid) {
  ring_coords <- test_hole_matrix[test_hole_matrix[, "L1"] == rid, c("X", "Y")]
  cbind(ring_coords[, "X"] + 4, ring_coords[, "Y"])
})
transformed_2 <- sf::st_polygon(rings_2)

test_mp_hole <- sf::st_union(transformed_1, transformed_2)

# Combine test geometries into a single sf object
testbench <- sf::st_sf(
  name = c("Hancock County", "Simple", "Multi", "Single Hole", "Multi Hole"),
  geometry = st_sfc(test_geom_translated, test_simple, test_mp, test_hole, test_mp_hole)
)

# Test for correct values from `second_areal_moment()` and `nmi()` ----

test_that("second_areal_moment returns correct values", {
  observed <- second_areal_moment(testbench$geometry)
  expected <- c(0.23480628, 0.11458333, 1.57459077, 1.58210246, 14.18946959)
  
  expect_equal(observed, expected, tolerance = ATOL)
})

test_that("nmi returns correct value", {
  observed <- nmi(shape)
  expected <- 0.802796
  
  expect_equal(observed, expected, tolerance = ATOL)
})
