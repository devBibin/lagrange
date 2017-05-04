# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'mainwindow.ui'
#
# Created: Thu Mar 30 17:58:08 2017
#      by: PyQt5 UI code generator 5.2.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QApplication, QWidget, QLineEdit, QLabel, QGridLayout, QFileDialog, QMessageBox, QTableWidgetItem
from subprocess import Popen
import shlex as shl
import os

#self.layoutWidget.setGeometry(QtCore.QRect(0, 0, 421, 412)) - PARTICLES
#self.layoutWidget1.setGeometry(QtCore.QRect(0, 0, 403, 102)) - MENU
#self.layoutWidget2.setGeometry(QtCore.QRect(0, 0, 391, 301)) - Manually
#self.layoutWidget3.setGeometry(QtCore.QRect(0, 0, 391, 241)) - FROMFILE



class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName("Lagrange")
        MainWindow.setEnabled(True)
        MainWindow.resize(430, 464)
        self.MainWindow = MainWindow
        self.centralwidget = QtWidgets.QWidget(MainWindow)
        self.centralwidget.setObjectName("centralwidget")
                #MENU
        self.layoutWidget1 = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget1.setGeometry(QtCore.QRect(0, 0, 403, 102))
        self.layoutWidget1.setObjectName("layoutWidget1")
        self.Menu = QtWidgets.QGridLayout(self.layoutWidget1)
        self.Menu.setContentsMargins(0, 0, 0, 0)
        self.Menu.setObjectName("Menu")
        self.Menu_0 = QtWidgets.QGridLayout()
        self.Menu_0.setObjectName("Menu_0")
        self.btnChoose = QtWidgets.QPushButton(self.layoutWidget1)
        self.btnChoose.setObjectName("btnChoose")
        self.Menu_0.addWidget(self.btnChoose, 3, 0, 1, 1)
        self.rbtnManually = QtWidgets.QRadioButton(self.layoutWidget1)
        self.rbtnManually.setEnabled(True)
        self.rbtnManually.setChecked(True)
        self.rbtnManually.setObjectName("rbtnManually")
        self.Menu_0.addWidget(self.rbtnManually, 1, 0, 1, 1)
        self.rbtnFromFile = QtWidgets.QRadioButton(self.layoutWidget1)
        self.rbtnFromFile.setEnabled(True)
        self.rbtnFromFile.setObjectName("rbtnFromFile")
        self.Menu_0.addWidget(self.rbtnFromFile, 2, 0, 1, 1)
        self.label_12 = QtWidgets.QLabel(self.layoutWidget1)
        self.label_12.setEnabled(True)
        self.label_12.setObjectName("label_12")
        self.Menu_0.addWidget(self.label_12, 0, 0, 1, 1)
        self.Menu.addLayout(self.Menu_0, 0, 0, 1, 1)
        self.btnSetParticles = QtWidgets.QPushButton(self.layoutWidget1)
        self.btnSetParticles.setEnabled(True)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnSetParticles.sizePolicy().hasHeightForWidth())
        self.btnSetParticles.setSizePolicy(sizePolicy)
        self.btnSetParticles.setObjectName("btnSetParticles")
        self.Menu.addWidget(self.btnSetParticles, 0, 1, 1, 1)
        # PARTICLES
        self.layoutWidget = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget.setGeometry(QtCore.QRect(100000, 1000000, 421, 412))
        self.layoutWidget.setObjectName("layoutWidget")
        self.Particles = QtWidgets.QGridLayout(self.layoutWidget)
        self.Particles.setContentsMargins(0, 0, 0, 0)
        self.Particles.setObjectName("Particles")
        self.tblParticles = QtWidgets.QTableWidget(self.layoutWidget)
        self.tblParticles.setObjectName("tblParticles")
        self.Particles.addWidget(self.tblParticles, 1, 0, 1, 1)
        self.gridLayout_12 = QtWidgets.QGridLayout()
        self.gridLayout_12.setObjectName("gridLayout_12")
        self.lnFileParticles = QtWidgets.QLineEdit(self.layoutWidget)
        self.lnFileParticles.setObjectName("lnFileParticles")
        self.gridLayout_12.addWidget(self.lnFileParticles, 1, 0, 1, 1)
        self.btnFileParticles = QtWidgets.QPushButton(self.layoutWidget)
        self.btnFileParticles.setObjectName("btnFileParticles")
        self.gridLayout_12.addWidget(self.btnFileParticles, 1, 1, 1, 1)
        self.btnLoadParticles = QtWidgets.QPushButton(self.layoutWidget)
        self.btnLoadParticles.setObjectName("btnLoadParticles")
        self.gridLayout_12.addWidget(self.btnLoadParticles, 3, 0, 1, 2)
        self.btnSaveParticles = QtWidgets.QPushButton(self.layoutWidget)
        self.btnSaveParticles.setObjectName("btnSaveParticles")
        self.gridLayout_12.addWidget(self.btnSaveParticles, 2, 1, 1, 1)
        self.btnCleanParticles = QtWidgets.QPushButton(self.layoutWidget)
        self.btnCleanParticles.setObjectName("btnCleanParticles")
        self.gridLayout_12.addWidget(self.btnCleanParticles, 2, 0, 1, 1)
        self.btnBack3 = QtWidgets.QPushButton(self.layoutWidget)
        self.btnBack3.setObjectName("btnBack3")
        self.gridLayout_12.addWidget(self.btnBack3, 4, 0, 1, 2)
        self.Particles.addLayout(self.gridLayout_12, 2, 0, 1, 1)
        self.label_15 = QtWidgets.QLabel(self.layoutWidget)
        self.label_15.setObjectName("label_15")
        self.Particles.addWidget(self.label_15, 0, 0, 1, 1)
        # MANUALLY
        self.layoutWidget2 = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget2.setGeometry(QtCore.QRect(100000, 100000, 391, 301))
        self.layoutWidget2.setObjectName("layoutWidget2")
        self.Manually = QtWidgets.QGridLayout(self.layoutWidget2)
        self.Manually.setContentsMargins(0, 0, 0, 0)
        self.Manually.setObjectName("Manually")
        self.gridLayout_15 = QtWidgets.QGridLayout()
        self.gridLayout_15.setObjectName("gridLayout_15")
        self.gridLayout_14 = QtWidgets.QGridLayout()
        self.gridLayout_14.setObjectName("gridLayout_14")
        self.gridLayout_5 = QtWidgets.QGridLayout()
        self.gridLayout_5.setObjectName("gridLayout_5")
        self.label_14 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_14.setObjectName("label_14")
        self.gridLayout_5.addWidget(self.label_14, 0, 0, 1, 1)
        self.gridLayout_3 = QtWidgets.QGridLayout()
        self.gridLayout_3.setObjectName("gridLayout_3")
        self.label = QtWidgets.QLabel(self.layoutWidget2)
        self.label.setEnabled(True)
        self.label.setObjectName("label")
        self.gridLayout_3.addWidget(self.label, 0, 0, 1, 1)
        self.lnWidth = QtWidgets.QSpinBox(self.layoutWidget2)
        self.lnWidth.setMinimum(10)
        self.lnWidth.setMaximum(1000)
        self.lnWidth.setObjectName("lnWidth")
        self.gridLayout_3.addWidget(self.lnWidth, 0, 1, 1, 1)
        self.label_2 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_2.setEnabled(True)
        self.label_2.setObjectName("label_2")
        self.gridLayout_3.addWidget(self.label_2, 0, 2, 1, 1)
        self.lnHeight = QtWidgets.QSpinBox(self.layoutWidget2)
        self.lnHeight.setMinimum(10)
        self.lnHeight.setMaximum(1000)
        self.lnHeight.setObjectName("lnHeight")
        self.gridLayout_3.addWidget(self.lnHeight, 0, 3, 1, 1)
        self.gridLayout_5.addLayout(self.gridLayout_3, 1, 0, 1, 1)
        self.gridLayout_14.addLayout(self.gridLayout_5, 0, 0, 1, 1)
        self.gridLayout_2 = QtWidgets.QGridLayout()
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.label_5 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_5.setEnabled(True)
        self.label_5.setObjectName("label_5")
        self.gridLayout_2.addWidget(self.label_5, 0, 0, 1, 1)
        self.gridLayout = QtWidgets.QGridLayout()
        self.gridLayout.setObjectName("gridLayout")
        self.label_6 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_6.setEnabled(True)
        self.label_6.setObjectName("label_6")
        self.gridLayout.addWidget(self.label_6, 0, 0, 1, 1)
        self.lnLongit = QtWidgets.QDoubleSpinBox(self.layoutWidget2)
        self.lnLongit.setObjectName("lnLongit")
        self.gridLayout.addWidget(self.lnLongit, 0, 1, 1, 1)
        self.label_7 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_7.setEnabled(True)
        self.label_7.setObjectName("label_7")
        self.gridLayout.addWidget(self.label_7, 0, 2, 1, 1)
        self.lnLatit = QtWidgets.QDoubleSpinBox(self.layoutWidget2)
        self.lnLatit.setObjectName("lnLatit")
        self.gridLayout.addWidget(self.lnLatit, 0, 3, 1, 1)
        self.gridLayout_2.addLayout(self.gridLayout, 1, 0, 1, 1)
        self.gridLayout_14.addLayout(self.gridLayout_2, 1, 0, 1, 1)
        self.gridLayout_15.addLayout(self.gridLayout_14, 0, 0, 1, 1)
        self.gridLayout_13 = QtWidgets.QGridLayout()
        self.gridLayout_13.setObjectName("gridLayout_13")
        self.gridLayout_11 = QtWidgets.QGridLayout()
        self.gridLayout_11.setObjectName("gridLayout_11")
        self.gridLayout_7 = QtWidgets.QGridLayout()
        self.gridLayout_7.setObjectName("gridLayout_7")
        self.label_3 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_3.setEnabled(True)
        self.label_3.setObjectName("label_3")
        self.gridLayout_7.addWidget(self.label_3, 0, 0, 1, 1)
        self.lnPrcCountM = QtWidgets.QSpinBox(self.layoutWidget2)
        self.lnPrcCountM.setMinimum(1)
        self.lnPrcCountM.setObjectName("lnPrcCountM")
        self.gridLayout_7.addWidget(self.lnPrcCountM, 0, 1, 1, 1)
        self.gridLayout_11.addLayout(self.gridLayout_7, 0, 0, 1, 1)
        self.gridLayout_8 = QtWidgets.QGridLayout()
        self.gridLayout_8.setObjectName("gridLayout_8")
        self.label_9 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_9.setEnabled(True)
        self.label_9.setObjectName("label_9")
        self.gridLayout_8.addWidget(self.label_9, 0, 0, 1, 1)
        self.lnMainProcM = QtWidgets.QSpinBox(self.layoutWidget2)
        self.lnMainProcM.setObjectName("lnMainProcM")
        self.gridLayout_8.addWidget(self.lnMainProcM, 0, 1, 1, 1)
        self.gridLayout_11.addLayout(self.gridLayout_8, 1, 0, 1, 1)
        self.gridLayout_9 = QtWidgets.QGridLayout()
        self.gridLayout_9.setObjectName("gridLayout_9")
        self.label_8 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_8.setObjectName("label_8")
        self.gridLayout_9.addWidget(self.label_8, 0, 0, 1, 1)
        self.cmbLandModeM = QtWidgets.QComboBox(self.layoutWidget2)
        self.cmbLandModeM.setObjectName("cmbLandModeM")
        self.cmbLandModeM.addItem("")
        self.cmbLandModeM.addItem("")
        self.gridLayout_9.addWidget(self.cmbLandModeM, 0, 1, 1, 1)
        self.gridLayout_11.addLayout(self.gridLayout_9, 2, 0, 1, 1)
        self.gridLayout_10 = QtWidgets.QGridLayout()
        self.gridLayout_10.setObjectName("gridLayout_10")
        self.label_16 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_16.setObjectName("label_16")
        self.gridLayout_10.addWidget(self.label_16, 0, 0, 1, 1)
        self.spinBox_2 = QtWidgets.QSpinBox(self.layoutWidget2)
        self.spinBox_2.setMinimum(1)
        self.spinBox_2.setMaximum(1000)
        self.spinBox_2.setObjectName("spinBox_2")
        self.gridLayout_10.addWidget(self.spinBox_2, 0, 1, 1, 1)
        self.gridLayout_11.addLayout(self.gridLayout_10, 3, 0, 1, 1)
        self.gridLayout_13.addLayout(self.gridLayout_11, 0, 0, 1, 1)
        self.gridLayout_6 = QtWidgets.QGridLayout()
        self.gridLayout_6.setObjectName("gridLayout_6")
        self.label_4 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_4.setObjectName("label_4")
        self.gridLayout_6.addWidget(self.label_4, 0, 0, 1, 1)
        self.lnGridStep = QtWidgets.QDoubleSpinBox(self.layoutWidget2)
        self.lnGridStep.setMinimum(0.01)
        self.lnGridStep.setMaximum(1.0)
        self.lnGridStep.setObjectName("lnGridStep")
        self.gridLayout_6.addWidget(self.lnGridStep, 0, 1, 1, 2)
        self.label_10 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_10.setObjectName("label_10")
        self.gridLayout_6.addWidget(self.label_10, 1, 0, 1, 1)
        self.label_11 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_11.setEnabled(True)
        self.label_11.setObjectName("label_11")
        self.gridLayout_6.addWidget(self.label_11, 2, 0, 1, 2)
        self.lnCountStep = QtWidgets.QSpinBox(self.layoutWidget2)
        self.lnCountStep.setObjectName("lnCountStep")
        self.lnCountStep.setMinimum(10)
        self.lnCountStep.setMaximum(1000)
        self.gridLayout_6.addWidget(self.lnCountStep, 2, 2, 1, 1)
        self.lnTimestep = QtWidgets.QSpinBox(self.layoutWidget2)
        self.lnTimestep.setMinimum(100)
        self.lnTimestep.setMaximum(10000)
        self.lnTimestep.setObjectName("lnTimestep")
        self.gridLayout_6.addWidget(self.lnTimestep, 1, 1, 1, 2)
        self.gridLayout_13.addLayout(self.gridLayout_6, 0, 1, 1, 1)
        self.gridLayout_15.addLayout(self.gridLayout_13, 1, 0, 1, 1)
        self.Manually.addLayout(self.gridLayout_15, 0, 0, 1, 1)
        self.gridLayout_16 = QtWidgets.QGridLayout()
        self.gridLayout_16.setObjectName("gridLayout_16")
        self.btnCalculate_manually = QtWidgets.QPushButton(self.layoutWidget2)
        self.btnCalculate_manually.setEnabled(True)
        self.btnCalculate_manually.setObjectName("btnCalculate_manually")
        self.gridLayout_16.addWidget(self.btnCalculate_manually, 0, 0, 1, 1)
        self.btnBack1 = QtWidgets.QPushButton(self.layoutWidget2)
        self.btnBack1.setObjectName("btnBack1")
        self.gridLayout_16.addWidget(self.btnBack1, 1, 0, 1, 1)
        self.Manually.addLayout(self.gridLayout_16, 1, 0, 1, 1)
        # FROM FILE
        self.layoutWidget3 = QtWidgets.QWidget(self.centralwidget)
        self.layoutWidget3.setGeometry(QtCore.QRect(100000, 100000, 391, 241))
        self.layoutWidget3.setObjectName("layoutWidget3")
        self.fromFile = QtWidgets.QGridLayout(self.layoutWidget3)
        self.fromFile.setContentsMargins(0, 0, 0, 0)
        self.fromFile.setObjectName("fromFile")
        self.gridLayout_23 = QtWidgets.QGridLayout()
        self.gridLayout_23.setObjectName("gridLayout_23")
        self.label_13 = QtWidgets.QLabel(self.layoutWidget3)
        self.label_13.setObjectName("label_13")
        self.gridLayout_23.addWidget(self.label_13, 0, 0, 1, 1)
        self.gridLayout_22 = QtWidgets.QGridLayout()
        self.gridLayout_22.setObjectName("gridLayout_22")
        self.gridLayout_21 = QtWidgets.QGridLayout()
        self.gridLayout_21.setObjectName("gridLayout_21")
        self.lnCurrentsFile = QtWidgets.QLineEdit(self.layoutWidget3)
        self.lnCurrentsFile.setObjectName("lnCurrentsFile")
        self.gridLayout_21.addWidget(self.lnCurrentsFile, 0, 0, 1, 1)
        self.btnFile_currents = QtWidgets.QPushButton(self.layoutWidget3)
        self.btnFile_currents.setObjectName("btnFile_currents")
        self.gridLayout_21.addWidget(self.btnFile_currents, 0, 1, 1, 1)
        self.gridLayout_22.addLayout(self.gridLayout_21, 0, 0, 1, 1)
        self.gridLayout_20 = QtWidgets.QGridLayout()
        self.gridLayout_20.setObjectName("gridLayout_20")
        self.gridLayout_4 = QtWidgets.QGridLayout()
        self.gridLayout_4.setObjectName("gridLayout_4")
        self.label_19 = QtWidgets.QLabel(self.layoutWidget3)
        self.label_19.setEnabled(True)
        self.label_19.setObjectName("label_19")
        self.gridLayout_4.addWidget(self.label_19, 0, 0, 1, 1)
        self.lnPrcCountF = QtWidgets.QSpinBox(self.layoutWidget3)
        self.lnPrcCountF.setMinimum(1)
        self.lnPrcCountF.setObjectName("lnPrcCountF")
        self.gridLayout_4.addWidget(self.lnPrcCountF, 0, 1, 1, 1)
        self.gridLayout_20.addLayout(self.gridLayout_4, 0, 0, 1, 1)
        self.gridLayout_17 = QtWidgets.QGridLayout()
        self.gridLayout_17.setObjectName("gridLayout_17")
        self.label_17 = QtWidgets.QLabel(self.layoutWidget3)
        self.label_17.setEnabled(True)
        self.label_17.setObjectName("label_17")
        self.gridLayout_17.addWidget(self.label_17, 0, 0, 1, 1)
        self.lnMainProcF_2 = QtWidgets.QSpinBox(self.layoutWidget3)
        self.lnMainProcF_2.setMinimum(0)
        self.lnMainProcF_2.setProperty("value", 0)
        self.lnMainProcF_2.setObjectName("lnMainProcF_2")
        self.gridLayout_17.addWidget(self.lnMainProcF_2, 0, 1, 1, 1)
        self.gridLayout_20.addLayout(self.gridLayout_17, 1, 0, 1, 1)
        self.gridLayout_18 = QtWidgets.QGridLayout()
        self.gridLayout_18.setObjectName("gridLayout_18")
        self.label_20 = QtWidgets.QLabel(self.layoutWidget3)
        self.label_20.setObjectName("label_20")
        self.gridLayout_18.addWidget(self.label_20, 0, 0, 1, 1)
        self.spinBox_3 = QtWidgets.QSpinBox(self.layoutWidget3)
        self.spinBox_3.setMinimum(1)
        self.spinBox_3.setMaximum(1000)
        self.spinBox_3.setObjectName("spinBox_3")
        self.gridLayout_18.addWidget(self.spinBox_3, 0, 1, 1, 1)
        self.gridLayout_20.addLayout(self.gridLayout_18, 2, 0, 1, 1)
        self.gridLayout_19 = QtWidgets.QGridLayout()
        self.gridLayout_19.setObjectName("gridLayout_19")
        self.label_18 = QtWidgets.QLabel(self.layoutWidget3)
        self.label_18.setObjectName("label_18")
        self.gridLayout_19.addWidget(self.label_18, 0, 0, 1, 1)
        self.cmbLandModeF = QtWidgets.QComboBox(self.layoutWidget3)
        self.cmbLandModeF.setObjectName("cmbLandModeF")
        self.cmbLandModeF.addItem("")
        self.cmbLandModeF.addItem("")
        self.gridLayout_19.addWidget(self.cmbLandModeF, 0, 1, 1, 1)
        self.gridLayout_20.addLayout(self.gridLayout_19, 3, 0, 1, 1)
        self.gridLayout_22.addLayout(self.gridLayout_20, 1, 0, 1, 1)
        self.gridLayout_23.addLayout(self.gridLayout_22, 1, 0, 1, 1)
        self.fromFile.addLayout(self.gridLayout_23, 0, 0, 1, 1)
        self.gridLayout_24 = QtWidgets.QGridLayout()
        self.gridLayout_24.setObjectName("gridLayout_24")
        self.btnCalculate_ff = QtWidgets.QPushButton(self.layoutWidget3)
        self.btnCalculate_ff.setObjectName("btnCalculate_ff")
        self.gridLayout_24.addWidget(self.btnCalculate_ff, 0, 0, 1, 1)
        self.btnBack2 = QtWidgets.QPushButton(self.layoutWidget3)
        self.btnBack2.setObjectName("btnBack2")
        self.gridLayout_24.addWidget(self.btnBack2, 1, 0, 1, 1)
        self.fromFile.addLayout(self.gridLayout_24, 1, 0, 1, 1)
        MainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtWidgets.QMenuBar(MainWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 430, 20))
        self.menubar.setObjectName("menubar")
        MainWindow.setMenuBar(self.menubar)
        self.statusbar = QtWidgets.QStatusBar(MainWindow)
        self.statusbar.setObjectName("statusbar")
        MainWindow.setStatusBar(self.statusbar)
        self.mode = 0

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
        self.btnSaveParticles.clicked.connect(self.saveToFile)
        self.btnCleanParticles.clicked.connect(self.cleanFile)
        self.retranslateUi(MainWindow)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)


#Hide all widgets in layout
    def hide_layout(self, layout):
        elements = (layout.itemAt(i) for i in range(layout.count())) 
        for el in elements:
            if isinstance(el, QGridLayout):
                self.hide_layout(el)
            else:
                el.setGeometry(QtCore.QRect(0, 0, 0, 0))
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

# Define length of file
    def file_len(self, fname):
        i = -1
        with open(fname) as f:
            for i, l in enumerate(f):
                pass
        return i + 1

#Hide all layouts
    def move_layouts_all(self):
        self.layoutWidget.setGeometry(QtCore.QRect(100000, 100000, 421, 412))
        self.layoutWidget1.setGeometry(QtCore.QRect(1000000, 100000, 403, 102))
        self.layoutWidget2.setGeometry(QtCore.QRect(100000, 100000, 391, 301))
        self.layoutWidget3.setGeometry(QtCore.QRect(1000000, 100000, 391, 241))

#Show menu layout
    def backToMenu(self):
        self.move_layouts_all()
        self.layoutWidget1.setGeometry(QtCore.QRect(5, 5, 403, 102))

#Show particle layout
    def setParticles(self):
        self.move_layouts_all()
        self.layoutWidget.setGeometry(QtCore.QRect(5, 5, 421, 412))

#Set mode, how to get currents info
    def setMode(self):
        self.move_layouts_all()
        if (self.rbtnManually.isChecked()):
            self.layoutWidget2.setGeometry(QtCore.QRect(5, 5, 391, 301))
            self.mode = 0
        if (self.rbtnFromFile.isChecked()):
            self.layoutWidget3.setGeometry(QtCore.QRect(5, 5, 391, 241))
            self.mode = 1

#Choose file
    def showFileDialog(self):
        fname = QFileDialog.getOpenFileName(None, 'Choose map', '/home/vladimir/model/project/client')[0]
        print (fname)
        s = self.MainWindow.sender().text()
        print (s)
        if s.find('Currents File',0,len(s)) != -1:
            self.lnCurrentsFile.setText(fname)
        if s.find('Particles File', 0, len(s)) != -1:
            self.lnFileParticles.setText(fname)

# Write to config file
    def writeConfig(self):
        f = open("config","w")
        if (self.mode == 0):
            f.write(str(0)+'\n')
            f.write(str(self.lnWidth.text())+'\n')
            f.write(str(self.lnHeight.text())+'\n')
            f.write(str(self.lnLongit.text().replace(',','.'))+'\n')
            f.write(str(self.lnLatit.text().replace(',','.'))+'\n')
            f.write(str(self.lnGridStep.text().replace(',','.'))+'\n')
            f.write(str(self.lnCountStep.text())+'\n')
            f.write(str(self.lnTimestep.text())+'\n')
            f.write(str(self.lnMainProcM.text())+'\n')
            f.write(str(self.spinBox_2.text() + '\n'))
            print (str(self.spinBox_2.text() + '\n'))
            s = self.cmbLandModeM.currentText()
            print (s)
            if (s.find('Drift',0,len(s) != -1)):
                f.write(str(0) + '\n')
            else:
                f.write(str(1) + '\n')
            f.write(self.lnFileParticles.text()+'\n')
            f.write(str(self.file_len(self.lnFileParticles.text()))+'\n')
        else:
            f.write(str(1)+'\n')
        f.close()


# Create uneditable widget
    def createUnEditableItm(self, text):
        itm = QTableWidgetItem()
        itm.setText(str(text))
        itm = self.unEditItm(itm)
        return itm

# Create uneditable widget
    def createEditableItm(self, text):
        itm = QTableWidgetItem()
        itm.setText(str(text))
        return itm

# Make widet uneditable
    def unEditItm(self, itm):
        flg = QtCore.Qt.ItemIsUserCheckable | QtCore.Qt.ItemIsEnabled
        itm.setFlags(flg)
        return itm


# Show particles' coordinates in table
    def showInTable(self):
        self.writeConfig()
        try:
            f = open(self.lnFileParticles.text(),"r+")
            i = 0
            self.tblParticles.setRowCount(1)
            self.tblParticles.setColumnCount(3)
            for line in f.readlines():
                x, y = line.split(" ")
                self.tblParticles.setItem(i,0,self.createUnEditableItm(i+1))
                self.tblParticles.setItem(i,1,self.createUnEditableItm(x))
                self.tblParticles.setItem(i,2,self.createUnEditableItm(y))
                i = i + 1
                self.tblParticles.setRowCount(i+1)
            self.tblParticles.setItem(i,0,self.createUnEditableItm(i+1))
            self.tblParticles.setItem(i,1,self.createEditableItm(''))
            self.tblParticles.setItem(i,2,self.createEditableItm(''))
        except Exception as e:
            print (e)

# Save last row of particles' table in file
    def saveToFile(self):
        try:
            f = open(self.lnFileParticles.text(),"a")
            self.tblParticles.setItem(self.tblParticles.rowCount() - 1,0,self.createUnEditableItm(self.tblParticles.rowCount()))
            x = self.tblParticles.item(self.tblParticles.rowCount() - 1, 1).text()
            y = self.tblParticles.item(self.tblParticles.rowCount() - 1, 2).text()
            f.write('%s %s\n' % (x,y))
            f.close()
            self.showInTable()
        except Exception as e:
            print (e)

# Clean files particle file
    def cleanFile(self):
        try:
            f = open(self.lnFileParticles.text(),"w")
            f.close()
            self.showInTable()
        except Exception as e:
            print (e)

# Calculate in anticyclone's field particles' tracks
    def calculateManually(self):
        self.writeConfig()
        args = shl.split('mpirun -np '+str(self.lnPrcCountM.text())+' ./main')
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
        self.writeConfig()
        print("fromFile")


    def retranslateUi(self, MainWindow):
        _translate = QtCore.QCoreApplication.translate
        MainWindow.setWindowTitle(_translate("MainWindow", "MainWindow"))
        self.btnFileParticles.setText(_translate("MainWindow", "Particles File"))
        self.btnLoadParticles.setText(_translate("MainWindow", "Load particles\' coordinates"))
        self.btnSaveParticles.setText(_translate("MainWindow", "Save particles"))
        self.btnCleanParticles.setText(_translate("MainWindow", "Clean particles"))
        self.btnBack3.setText(_translate("MainWindow", "Back to menu"))
        self.label_15.setText(_translate("MainWindow", "Set particles\' configuration."))
        self.btnChoose.setText(_translate("MainWindow", "Choose"))
        self.rbtnManually.setText(_translate("MainWindow", "Manually"))
        self.rbtnFromFile.setText(_translate("MainWindow", "From file"))
        self.lnFileParticles.setText(_translate("MainWindow", "/home/vladimir/model/project/client/particles.txt"))
        self.label_12.setText(_translate("MainWindow", "The way of getting info about currents:"))
        self.btnSetParticles.setText(_translate("MainWindow", "Set particles coordinates"))
        self.label_14.setText(_translate("MainWindow", "Count of nodes."))
        self.label.setText(_translate("MainWindow", "Width of grid:"))
        self.label_2.setText(_translate("MainWindow", "Height of grid:"))
        self.label_5.setText(_translate("MainWindow", "South-East point coordintes."))
        self.label_6.setText(_translate("MainWindow", "Longitude:"))
        self.label_7.setText(_translate("MainWindow", "Latitude:"))
        self.label_3.setText(_translate("MainWindow", "Processes count:"))
        self.label_9.setText(_translate("MainWindow", "Main process index:"))
        self.label_8.setText(_translate("MainWindow", "Land mode:"))
        self.cmbLandModeM.setItemText(0, _translate("MainWindow", "Drift"))
        self.cmbLandModeM.setItemText(1, _translate("MainWindow", "Stay"))
        self.label_16.setText(_translate("MainWindow", "Frequency for file"))
        self.label_4.setText(_translate("MainWindow", "Grid step:"))
        self.label_10.setText(_translate("MainWindow", "Time step:"))
        self.label_11.setText(_translate("MainWindow", "Count of steps:"))
        self.btnCalculate_manually.setText(_translate("MainWindow", "Calculate"))
        self.btnBack1.setText(_translate("MainWindow", "Back to menu"))
        self.label_13.setText(_translate("MainWindow", "Set currents configuration."))
        self.btnFile_currents.setText(_translate("MainWindow", "Currents File"))
        self.label_19.setText(_translate("MainWindow", "Processes count:"))
        self.label_17.setText(_translate("MainWindow", "Main process index:"))
        self.label_20.setText(_translate("MainWindow", "Frequency of write to file"))
        self.label_18.setText(_translate("MainWindow", "Land mode:"))
        self.cmbLandModeF.setItemText(0, _translate("MainWindow", "Drift"))
        self.cmbLandModeF.setItemText(1, _translate("MainWindow", "Stay"))
        self.btnCalculate_ff.setText(_translate("MainWindow", "Calculate"))
        self.btnBack2.setText(_translate("MainWindow", "Back to menu"))

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    MainWindow = QtWidgets.QMainWindow()
    ui = Ui_MainWindow()
    ui.setupUi(MainWindow)
    MainWindow.show()
    sys.exit(app.exec_())

