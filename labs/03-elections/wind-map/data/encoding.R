# ---------------------------------------------------------------------------
# THE ENCODING.  Declared here, once, and sourced by build-data.R, build-1620.R
# and reencode.R, so there is exactly one definition of what an arrow means.
#
# These are CONSTANTS, not quantities derived from the data: that is what makes
# the scale absolute rather than renormalised per panel.
#
# ONE RULE, EVERY MAP.  The angle IS the measurement and the length IS the
# measurement: rotation is proportional to the swing and length is linear in
# it, on all four figures.  A reader learns the encoding once, and both
# channels can be read back into a number rather than a direction.
# ---------------------------------------------------------------------------

DEG_PER_POINT <- 8      # degrees of rotation per point of margin swing
ANGLE_CAP     <- 90     # rotation saturates here (due east / due west)

# THREE CHANNELS, THREE FACTS.  Nothing is encoded twice.
#
#   ANGLE   the margin change, DEG_PER_POINT degrees per point, clockwise
#           toward the Republican.  Straight up is no change; a shift of
#           90/DEG_PER_POINT points lies flat and nothing rotates past flat.
#   COLOUR  which party the change favoured.
#   LENGTH  how many votes moved -- NOT how many points.
#
# Angle already carries the rate, so spending length on it too would say the
# same thing twice and leave the size of a place unsaid.  Length instead
# carries the quantity: |swing| x two-party votes, the net votes that changed
# hands.  A ten-point shift in a county of nine hundred and a ten-point shift
# in a county of nine hundred thousand are the same arrow under a per-point
# rule and are not remotely the same event; here the second is long and the
# first is short, and the map stops giving rural counties the same weight as
# metropolitan ones.  Stroke width is uniform for the same reason -- there is
# nothing left for it to say.
#
# THE SCALE.  Net votes moved runs from nothing to 400,096 (Los Angeles), which
# is 1,120 times the median county.  Straight proportion would draw the median
# arrow at a fiftieth of a pixel, so length is the SQUARE ROOT of net votes,
# reaching full extension at NET_VOTES_FULL and never shorter than LEN_MIN_FRAC
# of full.  The floor is what keeps a small county visible as an arrow rather
# than a speck; the cap is what stops one county setting the scale for three
# thousand.  Both are absolute constants printed in the legend, not quantiles
# of whatever data is in front of them.
LEN_MIN_FRAC  <- 0.15

# Kilometres of arrow at full extension, and the net-vote count that reaches it.
LEN_MAX_KM     <- c(us = 150,   ga_precinct = 30,  ga_county = 55)
NET_VOTES_FULL <- c(us = 25000, ga_precinct = 600, ga_county = 8000)

# swing (points, + = toward the Republican) and votes -> geometry
wind_geom <- function(swing, votes, map) {
  len_max <- as.numeric(LEN_MAX_KM[[map]])
  full    <- as.numeric(NET_VOTES_FULL[[map]])
  ang_deg <- pmax(-ANGLE_CAP, pmin(ANGLE_CAP, DEG_PER_POINT * swing))
  net     <- abs(swing) * votes / 100                    # net votes moved
  len_km  <- len_max * (LEN_MIN_FRAC +
               (1 - LEN_MIN_FRAC) * sqrt(pmin(net, full) / full))
  # 0 degrees = due north; positive rotates clockwise (toward the east/right)
  theta <- ang_deg * pi / 180
  data.frame(angle_deg = ang_deg, net_votes = net, len_km = len_km,
             dx = len_km * sin(theta), dy = len_km * cos(theta),
             capped_angle = abs(DEG_PER_POINT * swing) > ANGLE_CAP,
             capped_len   = net > full)
}

# The length a legend swatch for a given net-vote count has to be.
legend_len <- function(net, map) {
  len_max <- as.numeric(LEN_MAX_KM[[map]])
  full    <- as.numeric(NET_VOTES_FULL[[map]])
  len_max * (LEN_MIN_FRAC + (1 - LEN_MIN_FRAC) * sqrt(pmin(net, full) / full))
}
