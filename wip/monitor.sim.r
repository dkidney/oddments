## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title Monitor the progress of a simulation
# @description Use to track the progress of any loop which saves a file in a
#   target directory in each iteration. E.g. useful to track the progress of
#   simulations or bootstraps run using \code{parLapply\{parallel\}} where the
#   output of a model is saved as an \code{.RData} file in each iteration.
# @details A progress bar shows the percentage of files that have been saved
#   (out of a total of \code{nsims} files) and gives an estimate of the finish
#   time. Needs to be called in a separate \code{R} session to the one running
#   the loop.
# @param nsims number of iterations
# @param path directory where results files are stored
# @param refresh refresh rate of the progress bar (in seconds)
# @return TODO
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @examples
# pb = monitor.sim(nsims = 100, path = "K:/Archive/temp")
# close(pb)
# @export
monitor.sim = function(nsims, path, refresh = 5){
    # number of complete sims
    initial = completed = length(dir(path))
    # function to make progress bar label
    label.func = function(completed, nsims){
        paste(completed, " of ", nsims, " (", round(100 * completed / nsims, 2), " %) complete", sep = "")
    }
    # make a progress bar
    myProgressBar = winProgressBar(title   = "",
                                   label   = label.func(completed, nsims),
                                   min     = 0,
                                   max     = nsims,
                                   initial = completed,
                                   width   = 450)
    # keep checking if sim not complete
    start = Sys.time()
    while(completed < nsims){
        Sys.sleep(refresh)
        completed = length(dir(path))
        elapsed = Sys.time() - start
        each = elapsed / (completed - initial)
        todo = nsims - completed
        finish = Sys.time() + todo * each
        # update progress bar
        setWinProgressBar(
            pb      = myProgressBar,
            value   = completed,
            title   = paste("Estimated finish time:", finish),
            label   = label.func(completed, nsims)
        )
    }
    # final progress bar update
    setWinProgressBar(
        pb      = myProgressBar,
        value   = nsims,
        title   = "",
        label   = "Finished!"
    )
    return(myProgressBar)
}

# pb = monitor.sim(nsims = 100, path = 'C:/Users/darren/AppData/Local/Temp/RtmpEXdeW0/S=2, K=4, detectfn=0, bearings=1, distances=0, nsims=100/gibbonsecr_fit')

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
