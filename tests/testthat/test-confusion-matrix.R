context("test-confusion-matrix")

if(requireNamespace("yardstick", quietly = TRUE)){
  
  library(oddments)
  
  pos = "pos"
  neg = "neg"
  levs = c(pos, neg)
  x = factor(sample(levs, 1000, replace = TRUE), levels = levs)
  y = factor(sample(levs, 1000, replace = TRUE), levels = levs)
  
  # yardstick
  df = data.frame(x = x, y = y)
  cm = yardstick::conf_mat(df, "x", "y", )
  cm$table
  tp = cm$table[1, 1]
  fp = cm$table[1, 2]
  fn = cm$table[2, 1]
  tn = cm$table[2, 2]
  args = list(tp = tp, fp = fp, fn = fn, tn = tn)
  
  # tpr, recall and sensitivity should all be the same
  expect_equal(
    yardstick::recall_vec(x, y),
    do_call(args, recall)
  )
  expect_equal(
    yardstick::sens_vec(x, y),
    do_call(args, sensitivity)
  )
  expect_equal(
    do_call(args, tpr),
    do_call(args, recall)
  )
  expect_equal(
    do_call(args, tpr),
    do_call(args, sensitivity)
  )
  
  # fpr = 1 - tnr
  expect_equal(
    do_call(args, fpr),
    1 - do_call(args, tnr)
  )
  
  # fnr = 1 - tpr
  expect_equal(
    do_call(args, fnr),
    1 - do_call(args, tpr)
  )
  
  expect_equal(
    yardstick::accuracy_vec(x, y),
    do_call(args, accuracy)
  )
  expect_equal(
    yardstick::precision_vec(x, y),
    do_call(args, precision)
  )
  expect_equal(
    yardstick::spec_vec(x, y),
    do_call(args, specificity)
  )
  
}
