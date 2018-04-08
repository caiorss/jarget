#!/bin/sh
DEPS=org.jfree/jfreechart/1.0.17
exec jarget script -ps="$DEPS" -- "$0" "$@"
!#

import org.jfree.chart.{ChartPanel, ChartFactory, JFreeChart, ChartUtilities}
import org.jfree.data.general.DefaultPieDataset

object Main{

  def main(args: Array[String]){
    val dataset = new DefaultPieDataset()

    dataset.setValue("A", 75)
    dataset.setValue("B", 10)
    dataset.setValue("C", 10)
    dataset.setValue("D", 5)

    val chart = ChartFactory.createPieChart(
       "Sample Pie Chart", // Title
       dataset,            // Dataset 
       true,               // Show legend
       true,               // Tooltips on
       false 
     )

    // Save chart to a png file
    //---------------------------
    ChartUtilities.saveChartAsPNG(new java.io.File("mychart.png"), chart, 500, 500)

     // Show Chart in a Java Swing Frame
     //--------------------------------------
    val frame = new javax.swing.JFrame()
    frame.add(new ChartPanel(chart))
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    frame.setSize(693, 513)
    frame.setTitle("Sample Pie Chart")
    frame.setVisible(true)
  }

}
