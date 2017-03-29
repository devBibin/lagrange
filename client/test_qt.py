# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'mainwindow.ui'
#
# Created: Thu Jan 19 00:19:00 2017
#      by: PyQt5 UI code generator 5.2.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QApplication, QWidget, QLineEdit, QLabel, QGridLayout, QFileDialog, QMessageBox
from subprocess import Popen
import shlex as shl
import os

class Ui_MainWindow(QWidget):

    def setupUi(self, MainWindow):
        MainWindow.setObjectName("MainWindow")
        MainWindow.setEnabled(True)
        MainWindow.resize(879, 506)
        self.centralwidget = QtWidgets.QWidget(MainWindow)
        self.centralwidget.setObjectName("centralwidget")
        self.layoutWidget = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget.setGeometry(QtCore.QRect(10, 20, 391, 253))
        self.layoutWidget.setObjectName("layoutWidget")
        self.Manually = QtWidgets.QGridLayout(self.layoutWidget)
        self.Manually.setContentsMargins(0, 0, 0, 0)
        self.Manually.setObjectName("Manually")
        self.gridLayout_3 = QtWidgets.QGridLayout()
        self.gridLayout_3.setObjectName("gridLayout_3")
        self.gridLayout_2 = QtWidgets.QGridLayout()
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.label = QtWidgets.QLabel(self.layoutWidget)
        self.label.setEnabled(True)
        self.label.setObjectName("label")
        self.gridLayout_2.addWidget(self.label, 1, 0, 1, 1)
        self.lnWidth = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnWidth.setEnabled(True)
        self.lnWidth.setObjectName("lnWidth")
        self.gridLayout_2.addWidget(self.lnWidth, 1, 1, 1, 1)
        self.lnHeight = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnHeight.setEnabled(True)
        self.lnHeight.setObjectName("lnHeight")
        self.gridLayout_2.addWidget(self.lnHeight, 1, 3, 1, 1)
        self.label_2 = QtWidgets.QLabel(self.layoutWidget)
        self.label_2.setEnabled(True)
        self.label_2.setObjectName("label_2")
        self.gridLayout_2.addWidget(self.label_2, 1, 2, 1, 1)
        self.label_14 = QtWidgets.QLabel(self.layoutWidget)
        self.label_14.setObjectName("label_14")
        self.gridLayout_2.addWidget(self.label_14, 0, 0, 1, 2)
        self.gridLayout_3.addLayout(self.gridLayout_2, 0, 0, 1, 2)
        self.gridLayout_4 = QtWidgets.QGridLayout()
        self.gridLayout_4.setObjectName("gridLayout_4")
        self.label_5 = QtWidgets.QLabel(self.layoutWidget)
        self.label_5.setEnabled(True)
        self.label_5.setObjectName("label_5")
        self.gridLayout_4.addWidget(self.label_5, 0, 0, 1, 1)
        self.gridLayout = QtWidgets.QGridLayout()
        self.gridLayout.setObjectName("gridLayout")
        self.label_6 = QtWidgets.QLabel(self.layoutWidget)
        self.label_6.setEnabled(True)
        self.label_6.setObjectName("label_6")
        self.gridLayout.addWidget(self.label_6, 0, 0, 1, 1)
        self.lnLongit = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnLongit.setEnabled(True)
        self.lnLongit.setObjectName("lnLongit")
        self.gridLayout.addWidget(self.lnLongit, 0, 1, 1, 1)
        self.label_7 = QtWidgets.QLabel(self.layoutWidget)
        self.label_7.setEnabled(True)
        self.label_7.setObjectName("label_7")
        self.gridLayout.addWidget(self.label_7, 0, 2, 1, 1)
        self.lnLatit = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnLatit.setEnabled(True)
        self.lnLatit.setObjectName("lnLatit")
        self.gridLayout.addWidget(self.lnLatit, 0, 3, 1, 1)
        self.gridLayout_4.addLayout(self.gridLayout, 1, 0, 1, 1)
        self.gridLayout_3.addLayout(self.gridLayout_4, 1, 0, 1, 2)
        self.gridLayout_5 = QtWidgets.QGridLayout()
        self.gridLayout_5.setObjectName("gridLayout_5")
        self.label_3 = QtWidgets.QLabel(self.layoutWidget)
        self.label_3.setEnabled(True)
        self.label_3.setObjectName("label_3")
        self.gridLayout_5.addWidget(self.label_3, 0, 0, 1, 1)
        self.cmbCountProc = QtWidgets.QComboBox(self.layoutWidget)
        self.cmbCountProc.setEnabled(True)
        self.cmbCountProc.setObjectName("cmbCountProc")
        self.cmbCountProc.addItem("")
        self.cmbCountProc.addItem("")
        self.cmbCountProc.addItem("")
        self.gridLayout_5.addWidget(self.cmbCountProc, 0, 1, 1, 1)
        self.label_9 = QtWidgets.QLabel(self.layoutWidget)
        self.label_9.setEnabled(True)
        self.label_9.setObjectName("label_9")
        self.gridLayout_5.addWidget(self.label_9, 1, 0, 1, 1)
        self.lnMainProc = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnMainProc.setEnabled(True)
        self.lnMainProc.setObjectName("lnMainProc")
        self.gridLayout_5.addWidget(self.lnMainProc, 1, 1, 1, 1)
        self.gridLayout_3.addLayout(self.gridLayout_5, 2, 0, 1, 1)
        self.gridLayout_8 = QtWidgets.QGridLayout()
        self.gridLayout_8.setObjectName("gridLayout_8")
        self.gridLayout_7 = QtWidgets.QGridLayout()
        self.gridLayout_7.setObjectName("gridLayout_7")
        self.label_10 = QtWidgets.QLabel(self.layoutWidget)
        self.label_10.setObjectName("label_10")
        self.gridLayout_7.addWidget(self.label_10, 1, 0, 1, 1)
        self.label_11 = QtWidgets.QLabel(self.layoutWidget)
        self.label_11.setEnabled(True)
        self.label_11.setObjectName("label_11")
        self.gridLayout_7.addWidget(self.label_11, 2, 0, 1, 1)
        self.label_4 = QtWidgets.QLabel(self.layoutWidget)
        self.label_4.setObjectName("label_4")
        self.gridLayout_7.addWidget(self.label_4, 0, 0, 1, 1)
        self.gridLayout_8.addLayout(self.gridLayout_7, 0, 0, 1, 1)
        self.gridLayout_6 = QtWidgets.QGridLayout()
        self.gridLayout_6.setObjectName("gridLayout_6")
        self.lnGridStep = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnGridStep.setEnabled(True)
        self.lnGridStep.setObjectName("lnGridStep")
        self.gridLayout_6.addWidget(self.lnGridStep, 0, 0, 1, 1)
        self.lnTImeStep = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnTImeStep.setEnabled(True)
        self.lnTImeStep.setObjectName("lnTImeStep")
        self.gridLayout_6.addWidget(self.lnTImeStep, 1, 0, 1, 1)
        self.lnCountStep = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnCountStep.setEnabled(True)
        self.lnCountStep.setObjectName("lnCountStep")
        self.gridLayout_6.addWidget(self.lnCountStep, 2, 0, 1, 1)
        self.gridLayout_8.addLayout(self.gridLayout_6, 0, 1, 1, 1)
        self.gridLayout_3.addLayout(self.gridLayout_8, 2, 1, 1, 1)
        self.Manually.addLayout(self.gridLayout_3, 0, 0, 1, 1)
        self.btnCalculate_manually = QtWidgets.QPushButton(self.layoutWidget)
        self.btnCalculate_manually.setEnabled(True)
        self.btnCalculate_manually.setObjectName("btnCalculate_manually")
        self.Manually.addWidget(self.btnCalculate_manually, 1, 0, 1, 1)
        self.btnBack1 = QtWidgets.QPushButton(self.layoutWidget)
        self.btnBack1.setObjectName("btnBack1")
        self.Manually.addWidget(self.btnBack1, 2, 0, 1, 1)
        self.layoutWidget1 = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget1.setGeometry(QtCore.QRect(10, 280, 391, 104))
        self.layoutWidget1.setObjectName("layoutWidget1")
        self.fromFile = QtWidgets.QGridLayout(self.layoutWidget1)
        self.fromFile.setContentsMargins(0, 0, 0, 0)
        self.fromFile.setObjectName("fromFile")
        self.label_13 = QtWidgets.QLabel(self.layoutWidget1)
        self.label_13.setObjectName("label_13")
        self.fromFile.addWidget(self.label_13, 0, 0, 1, 2)
        self.lnCurrentsFile = QtWidgets.QLineEdit(self.layoutWidget1)
        self.lnCurrentsFile.setObjectName("lnCurrentsFile")
        self.fromFile.addWidget(self.lnCurrentsFile, 1, 0, 1, 1)
        self.btnFile_currents = QtWidgets.QPushButton(self.layoutWidget1)
        self.btnFile_currents.setObjectName("btnFile_currents")
        self.fromFile.addWidget(self.btnFile_currents, 1, 1, 1, 1)
        self.btnCalculate_ff = QtWidgets.QPushButton(self.layoutWidget1)
        self.btnCalculate_ff.setObjectName("btnCalculate_ff")
        self.fromFile.addWidget(self.btnCalculate_ff, 2, 0, 1, 2)
        self.btnBack2 = QtWidgets.QPushButton(self.layoutWidget1)
        self.btnBack2.setObjectName("btnBack2")
        self.fromFile.addWidget(self.btnBack2, 3, 0, 1, 2)
        self.layoutWidget2 = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget2.setGeometry(QtCore.QRect(410, 130, 421, 304))
        self.layoutWidget2.setObjectName("layoutWidget2")
        self.Particles = QtWidgets.QGridLayout(self.layoutWidget2)
        self.Particles.setContentsMargins(0, 0, 0, 0)
        self.Particles.setObjectName("Particles")
        self.tblParticles = QtWidgets.QTableView(self.layoutWidget2)
        self.tblParticles.setObjectName("tblParticles")
        self.Particles.addWidget(self.tblParticles, 1, 0, 1, 1)
        self.gridLayout_12 = QtWidgets.QGridLayout()
        self.gridLayout_12.setObjectName("gridLayout_12")
        self.lnFileParticles = QtWidgets.QLineEdit(self.layoutWidget2)
        self.lnFileParticles.setObjectName("lnFileParticles")
        self.gridLayout_12.addWidget(self.lnFileParticles, 1, 0, 1, 1)
        self.btnFileParticles = QtWidgets.QPushButton(self.layoutWidget2)
        self.btnFileParticles.setObjectName("btnFileParticles")
        self.gridLayout_12.addWidget(self.btnFileParticles, 1, 1, 1, 1)
        self.btnLoadParticles = QtWidgets.QPushButton(self.layoutWidget2)
        self.btnLoadParticles.setObjectName("btnLoadParticles")
        self.gridLayout_12.addWidget(self.btnLoadParticles, 2, 0, 1, 2)
        self.btnBack3 = QtWidgets.QPushButton(self.layoutWidget2)
        self.btnBack3.setObjectName("btnBack3")
        self.gridLayout_12.addWidget(self.btnBack3, 3, 0, 1, 2)
        self.Particles.addLayout(self.gridLayout_12, 2, 0, 1, 1)
        self.label_15 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_15.setObjectName("label_15")
        self.Particles.addWidget(self.label_15, 0, 0, 1, 1)
        self.layoutWidget3 = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget3.setGeometry(QtCore.QRect(410, 20, 403, 102))
        self.layoutWidget3.setObjectName("layoutWidget3")
        self.Menu = QtWidgets.QGridLayout(self.layoutWidget3)
        self.Menu.setContentsMargins(0, 0, 0, 0)
        self.Menu.setObjectName("Menu")
        self.Menu_0 = QtWidgets.QGridLayout()
        self.Menu_0.setObjectName("Menu_0")
        self.btnChoose = QtWidgets.QPushButton(self.layoutWidget3)
        self.btnChoose.setObjectName("btnChoose")
        self.Menu_0.addWidget(self.btnChoose, 3, 0, 1, 1)
        self.label_12 = QtWidgets.QLabel(self.layoutWidget3)
        self.label_12.setEnabled(True)
        self.label_12.setObjectName("label_12")
        self.Menu_0.addWidget(self.label_12, 0, 0, 1, 1)
        self.rbtnManually = QtWidgets.QRadioButton(self.layoutWidget3)
        self.rbtnManually.setEnabled(True)
        self.rbtnManually.setChecked(True)
        self.rbtnManually.setObjectName("rbtnManually")
        self.Menu_0.addWidget(self.rbtnManually, 1, 0, 1, 1)
        self.rbtnFromFile = QtWidgets.QRadioButton(self.layoutWidget3)
        self.rbtnFromFile.setEnabled(True)
        self.rbtnFromFile.setObjectName("rbtnFromFile")
        self.Menu_0.addWidget(self.rbtnFromFile, 2, 0, 1, 1)
        self.Menu.addLayout(self.Menu_0, 0, 0, 1, 1)
        self.btnSetParticles = QtWidgets.QPushButton(self.layoutWidget3)
        self.btnSetParticles.setEnabled(True)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnSetParticles.sizePolicy().hasHeightForWidth())
        self.btnSetParticles.setSizePolicy(sizePolicy)
        self.btnSetParticles.setObjectName("btnSetParticles")
        self.Menu.addWidget(self.btnSetParticles, 0, 1, 1, 1)
        MainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtWidgets.QMenuBar(MainWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 879, 20))
        self.menubar.setObjectName("menubar")
        MainWindow.setMenuBar(self.menubar)
        self.statusbar = QtWidgets.QStatusBar(MainWindow)
        self.statusbar.setObjectName("statusbar")
        MainWindow.setStatusBar(self.statusbar)

        # Hide all widgets except menu
        self.hide_layout(self.Manually)
        self.hide_layout(self.fromFile)
        self.hide_layout(self.Particles)

        #Connect buttons with slots
        self.btnChoose.clicked.connect(self.setMode)
        self.btnBack1.clicked.connect(self.backToMenu)
        self.btnBack2.clicked.connect(self.backToMenu)
        self.btnBack3.clicked.connect(self.backToMenu)
        self.btnCalculate_manually.clicked.connect(self.calculateManually)
        self.btnCalculate_ff.clicked.connect(self.calculateFromFile)
        self.btnFileParticles.clicked.connect(self.showFileDialog)
        self.btnFile_currents.clicked.connect(self.showFileDialog)
        self.btnSetParticles.clicked.connect(self.setParticles)
        self.btnLoadParticles.clicked.connect(self.showInTable)

        self.retranslateUi(MainWindow)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)

#Hide all widgets in layout
    def hide_layout(self, layout):
        elements = (layout.itemAt(i) for i in range(layout.count())) 
        for el in elements:
            if isinstance(el, QGridLayout):
                self.hide_layout(el)
            else:
                el.widget().hide()

#Show all widgets in layot
    def show_layout(self, layout):
        elements = (layout.itemAt(i) for i in range(layout.count())) 
        for el in elements:
            print (el)
            if isinstance(el, QGridLayout):
                self.show_layout(el)
            else:
                el.widget().show()

#Hide all layouts
    def hide_layouts_all(self):
        self.hide_layout(self.Manually)
        self.hide_layout(self.fromFile)
        self.hide_layout(self.Particles)
        self.hide_layout(self.Menu)

#Show menu layout
    def backToMenu(self):
        self.hide_layouts_all()
        self.show_layout(self.Menu)

#Show particle layout
    def setParticles(self):
        self.hide_layouts_all()
        self.show_layout(self.Particles)

#Set mode, how to get currents info
    def setMode(self):
        self.hide_layouts_all()
        if (self.rbtnManually.isChecked()):
            self.show_layout(self.Manually)
            #MainWindow.resize(424, 449)
        if (self.rbtnFromFile.isChecked()):
            self.show_layout(self.fromFile)
            #MainWindow.resize(424, 100)

#Choose file
    def showFileDialog(self):
        fname = QFileDialog.getOpenFileName(self, 'Open file', '/home')[0]
        s = self.sender().text()
        if s.find('Currents File',0,len(s)) != -1:
            self.lnCurrentsFile.setText(fname)
        if s.find('Particles File', 0, len(s)) != -1:
            self.lnFileParticles.setText(fname)

    def showInTable(self):
        print("JOPA")
        filename = self.lnFileParticles.text()
        print(filename)
        try:
            print ("HEY")
            f = open(filename,"r+")
            print ("HEY")
            for line in f.readli():
                x, y = line.split(" ")
                print (x,y)
        except Exception as e:
            print (e)



    def calculateManually(self):
        f = open("config","w")
        f.write(str(0)+'\n')
        f.write(str(self.lnWidth.text())+'\n')
        f.write(str(self.lnHeight.text())+'\n')
        f.write(str(self.lnLongit.text())+'\n')
        f.write(str(self.lnLatit.text())+'\n')
        f.write(str(self.lnGridStep.text())+'\n')
        f.write(str(self.lnCountStep.text())+'\n')
        f.write(str(self.lnTImeStep.text())+'\n')
        f.write(str(self.lnMainProc.text())+'\n')
        f.close()
        args = shl.split('mpirun -np '+str(self.cmbCountProc.currentText())+' ./main')
        proc = Popen(                                                   #основной алгоритм
            args,
            cwd=r"../",
        )
        proc.wait()                                                         # дождаться выполнения
        args = shl.split('python graph.py')
        proc = Popen(                                                    #отрисовка	
            args,
            cwd=r"../tracks",
        )
        proc.wait()                                                         # дождаться выполнения

    def calculateFromFile(self):
        print("fromFile")

    def retranslateUi(self, MainWindow):
        _translate = QtCore.QCoreApplication.translate
        MainWindow.setWindowTitle(_translate("MainWindow", "MainWindow"))
        self.label.setText(_translate("MainWindow", "Width of grid:"))
        self.lnWidth.setText(_translate("MainWindow", "11"))
        self.lnHeight.setText(_translate("MainWindow", "11"))
        self.label_2.setText(_translate("MainWindow", "Height of grid:"))
        self.label_14.setText(_translate("MainWindow", "Count of nodes."))
        self.label_5.setText(_translate("MainWindow", "South-East point coordintes."))
        self.label_6.setText(_translate("MainWindow", "Longitude:"))
        self.lnLongit.setText(_translate("MainWindow", "33"))
        self.label_7.setText(_translate("MainWindow", "Latitude:"))
        self.lnLatit.setText(_translate("MainWindow", "33"))
        self.label_3.setText(_translate("MainWindow", "Processes count:"))
        self.cmbCountProc.setItemText(0, _translate("MainWindow", "1"))
        self.cmbCountProc.setItemText(1, _translate("MainWindow", "4"))
        self.cmbCountProc.setItemText(2, _translate("MainWindow", "9"))
        self.label_9.setText(_translate("MainWindow", "Main process index:"))
        self.lnMainProc.setText(_translate("MainWindow", "0"))
        self.label_10.setText(_translate("MainWindow", "Time step:"))
        self.label_11.setText(_translate("MainWindow", "Count of steps:"))
        self.label_4.setText(_translate("MainWindow", "Grid step:"))
        self.lnGridStep.setText(_translate("MainWindow", "0.01"))
        self.lnTImeStep.setText(_translate("MainWindow", "100"))
        self.lnCountStep.setText(_translate("MainWindow", "100"))
        self.btnCalculate_manually.setText(_translate("MainWindow", "Calculate"))
        self.btnBack1.setText(_translate("MainWindow", "Back to menu"))
        self.label_13.setText(_translate("MainWindow", "Set currents configuration."))
        self.btnFile_currents.setText(_translate("MainWindow", "Currents File"))
        self.btnCalculate_ff.setText(_translate("MainWindow", "Calculate"))
        self.btnBack2.setText(_translate("MainWindow", "Back to menu"))
        self.btnFileParticles.setText(_translate("MainWindow", "Particles File"))
        self.btnLoadParticles.setText(_translate("MainWindow", "Load particles\' coordinates"))
        self.btnBack3.setText(_translate("MainWindow", "Back to menu"))
        self.label_15.setText(_translate("MainWindow", "Set particles\' configuration."))
        self.btnChoose.setText(_translate("MainWindow", "Choose"))
        self.label_12.setText(_translate("MainWindow", "The way of getting info about currents:"))
        self.rbtnManually.setText(_translate("MainWindow", "Manually"))
        self.rbtnFromFile.setText(_translate("MainWindow", "From file"))
        self.btnSetParticles.setText(_translate("MainWindow", "Set particles coordinates"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    MainWindow = QtWidgets.QMainWindow()
    ui = Ui_MainWindow()
    ui.setupUi(MainWindow)
    MainWindow.show()
    sys.exit(app.exec_())

