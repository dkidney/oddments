# @title Score transformation onto bespoke scale
# @description Transforms a probability-scale score onto a bespoke scale. The bespoke
#   scale used in the past has typically between 200 and 300 (approx).
# @param y raw score
# @param pdo points to double the odds
# @param good ratio of goods to bads when the transformed score is equal to
#   \code{atscore}
# @param atscore value of the transformed score at which the ratio of goods to bads
#   is \code{good}:1
# @param inverse if \code{TRUE}, the inverse transform is applied
# @examples
# # random vector of scores on probability scale
# y = runif(1000)
#
# # apply the transformation (using default parameter values)
# y.trans = score_transform(y)
#
# # check that back-transformation equals the original
# all.equal(score_transform(y.trans, inverse = TRUE), y)
#
# # compare the raw and tranformed scores
# op = par(mfrow = c(1,2))
# hist(y, main = "Raw")
# hist(y.trans, main = "Transform")
# abline(v = 200, col = 2, lwd = 2) # red line shows atscore value
# par(op)
# @export

# score_transform = function(y, pdo = 20, good = 1, atscore = 200, inverse = FALSE){
#     factor = pdo / log(2)
#     offset = atscore - factor * log(good)
#     if(inverse){
#         1 - inv_logit((y - offset) / factor)
#     }else{
#         offset + factor * logit(1 - y)
#     }
# }

