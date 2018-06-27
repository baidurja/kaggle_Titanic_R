source( './preProcess/preProcess.R' )

dataset = readRawData( '../data/train.csv' )

plot_missing( dataset )
plot_bar( dataset )
plot_histogram( dataset )