context("test-confusion-matrix")

if(requireNamespace("yardstick", quietly = TRUE)){

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
    expect_equal(
        yardstick::accuracy_vec(x, y),
        do.call(oddments::accuracy, args)
    )
    expect_equal(
        yardstick::precision_vec(x, y),
        do.call(oddments::precision, args)
    )
    expect_equal(
        yardstick::recall_vec(x, y),
        do.call(oddments::recall, args)
    )
    expect_equal(
        yardstick::sens_vec(x, y),
        do.call(oddments::sensitivity, args)
    )
    expect_equal(
        yardstick::sens_vec(x, y),
        do.call(oddments::tpr, args)
    )
    expect_equal(
        yardstick::spec_vec(x, y),
        do.call(oddments::specificity, args)
    )

}
