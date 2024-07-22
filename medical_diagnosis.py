import sys
from PyQt5.QtWidgets import (
    QApplication, QMainWindow, QWidget, QVBoxLayout, QPushButton, QLabel, 
    QLineEdit, QMessageBox, QStackedWidget, QHBoxLayout, QFormLayout, QDialog, QComboBox, QTextBrowser,
    QCheckBox, QScrollArea, QListWidget, QAbstractItemView
)
from PyQt5.QtCore import Qt
from PyQt5.QtGui import QFont, QPainter, QLinearGradient, QBrush, QColor, QCursor
from pyswip import Prolog
from datetime import datetime


# Initialize Prolog
prolog = Prolog()
prolog.consult("mco_2.pl")


class DiseaseManager(QDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Disease Manager")
        self.setMinimumWidth(600)

        # Layout for the dialog
        self.layout = QVBoxLayout(self)

        # Layout for disease list and details
        self.details_layout = QHBoxLayout()
        self.layout.addLayout(self.details_layout)

        # List widget to display diseases
        self.disease_list = QListWidget()
        self.disease_list.currentItemChanged.connect(self.update_details)
        self.details_layout.addWidget(self.disease_list)

        # Widget to show disease details
        self.details_widget = QWidget()
        self.details_layout.addWidget(self.details_widget)

        # Layout for disease details
        self.details_inner_layout = QVBoxLayout(self.details_widget)

        # Input field for new diseases
        self.disease_name_input = QLineEdit()
        self.disease_name_input.setPlaceholderText("Enter disease name...")
        self.details_inner_layout.addWidget(self.disease_name_input)

        # Input field for new symptoms
        self.new_symptom_input = QLineEdit()
        self.new_symptom_input.setPlaceholderText("Enter new symptom...")
        self.details_inner_layout.addWidget(self.new_symptom_input)

        # Input field for new transmissions
        self.new_transmission_input = QLineEdit()
        self.new_transmission_input.setPlaceholderText("Enter new transmission...")
        self.details_inner_layout.addWidget(self.new_transmission_input)


        # ComboBox for existing symptoms and transmission
        self.symptom_combo = QComboBox()
        self.transmission_combo = QComboBox()
        self.details_inner_layout.addWidget(QLabel("Symptoms:"))
        self.details_inner_layout.addWidget(self.symptom_combo)
        self.details_inner_layout.addWidget(QLabel("Transmissions:"))
        self.details_inner_layout.addWidget(self.transmission_combo)

        # Buttons for CRUD operations
        self.create_button = QPushButton("Add Disease")
        self.update_button = QPushButton("Update Disease")
        self.delete_button = QPushButton("Delete Disease")
        self.add_symptom_button = QPushButton("Add Symptom to Disease")
        self.add_transmission_button = QPushButton("Add Transmission to Disease")
        self.remove_symptom_button = QPushButton("Remove Symptom from Disease")
        self.remove_transmission_button = QPushButton("Remove Transmission from Disease")

        # Add buttons to layout
        self.details_inner_layout.addWidget(self.create_button)
        self.details_inner_layout.addWidget(self.update_button)
        self.details_inner_layout.addWidget(self.delete_button)
        self.details_inner_layout.addWidget(self.add_symptom_button)
        self.details_inner_layout.addWidget(self.add_transmission_button)
        self.details_inner_layout.addWidget(self.remove_symptom_button)
        self.details_inner_layout.addWidget(self.remove_transmission_button)

        # Connect buttons to their actions
        self.create_button.clicked.connect(self.create_disease)
        self.update_button.clicked.connect(self.update_disease)
        self.delete_button.clicked.connect(self.delete_disease)
        self.add_symptom_button.clicked.connect(self.add_symptom_to_disease)
        self.add_transmission_button.clicked.connect(self.add_transmission_to_disease)
        self.remove_symptom_button.clicked.connect(self.remove_symptom_from_disease)
        self.remove_transmission_button.clicked.connect(self.remove_transmission_from_disease)

        # Load existing diseases
        self.populate_diseases()




    def populate_diseases(self):
        diseases = self.query_prolog("disease(Disease)")
        self.disease_list.clear()
        self.disease_list.addItems(diseases)
        self.disease_name_input.clear()
        self.new_symptom_input.clear()

    def populate_symptoms(self, disease_name):
        symptoms = self.query_prolog(f"has_symptom({disease_name}, Symptom)")
        self.symptom_combo.clear()
        self.symptom_combo.addItems(symptoms)

    def populate_transmissions(self, disease_name):
        transmissions = self.query_prolog(f"transmitted_by({disease_name}, Transmission)")
        self.transmission_combo.clear()
        self.transmission_combo.addItems(transmissions)

    def update_details(self, current_item):
        if current_item:
            disease_name = current_item.text().replace(" ", "_")
            self.disease_name_input.setText(disease_name.replace("_", " "))
            self.new_symptom_input.clear()
            self.populate_symptoms(disease_name)
            self.populate_transmissions(disease_name)

    def create_disease(self):
        disease_name = self.disease_name_input.text().strip()
        if disease_name:
            disease_name = disease_name.replace(" ", "_")
            existing_diseases = self.query_prolog(f"disease(Disease)")
            if disease_name not in existing_diseases:
                prolog.assertz(f"disease({disease_name})")
                self.populate_diseases()
                QMessageBox.information(self, "Success", "Disease added successfully!")
            else:
                QMessageBox.warning(self, "Warning", "Disease already exists.")
        else:
            QMessageBox.warning(self, "Input Error", "Disease name cannot be empty.")

    def update_disease(self):
        selected_item = self.disease_list.currentItem()
        if selected_item:
            old_disease_name = selected_item.text().replace(" ", "_")
            new_disease_name = self.disease_name_input.text().strip().replace(" ", "_")
            if new_disease_name:
                if old_disease_name != new_disease_name:
                    prolog.retract(f"disease({old_disease_name})")
                    prolog.assertz(f"disease({new_disease_name})")

                    self.populate_diseases()
                    QMessageBox.information(self, "Success", "Disease updated successfully!")
                else:
                    QMessageBox.warning(self, "Input Error", "New disease name must be different.")
            else:
                QMessageBox.warning(self, "Input Error", "Disease name cannot be empty.")
        else:
            QMessageBox.warning(self, "Selection Error", "No disease selected for update.")

    def delete_disease(self):
        selected_item = self.disease_list.currentItem()
        if selected_item:
            disease_name = selected_item.text().replace(" ", "_")
            prolog.retract(f"disease({disease_name})")
            self.remove_symptoms_from_disease(disease_name)
            self.remove_transmissions_from_disease(disease_name)
            self.populate_diseases()
            self.disease_name_input.clear()
            self.new_symptom_input.clear()
            self.symptom_combo.clear()
            self.transmission_combo.clear()
            QMessageBox.information(self, "Success", "Disease deleted successfully!")
        else:
            QMessageBox.warning(self, "Selection Error", "No disease selected for deletion.")

    def remove_symptoms_from_disease(self, disease_name):
        symptoms = self.query_prolog(f"has_symptom({disease_name}, Symptom)")
        for symptom in symptoms:
            prolog.retract(f"has_symptom({disease_name}, {symptom})")

    def remove_transmissions_from_disease(self, disease_name):
        transmissions = self.query_prolog(f"transmitted_by({disease_name}, Transmission)")
        for transmission in transmissions:
            prolog.retract(f"transmitted_by({disease_name}, {transmission})")

    def add_symptom_to_disease(self):
        selected_disease = self.disease_list.currentItem()
        new_symptom = self.new_symptom_input.text().strip().replace(" ", "_")

        if selected_disease:
            disease_name = selected_disease.text().replace(" ", "_")
            if new_symptom:
                if new_symptom not in self.query_prolog("symptom(Symptom)"):
                    prolog.assertz(f"symptom({new_symptom})")

                existing_symptoms = list(prolog.query(f"has_symptom({disease_name}, {new_symptom})"))
                
                if not existing_symptoms:
                    prolog.assertz(f"has_symptom({disease_name}, {new_symptom})")
                    self.populate_symptoms(disease_name)
                    self.new_symptom_input.clear()
                    QMessageBox.information(self, "Success", "New symptom added to disease successfully!")
                else:
                    QMessageBox.warning(self, "Warning", "Symptom already associated with the disease.")
         
            else:
                QMessageBox.warning(self, "Input Error", "No symptom specified.")
        else:
            QMessageBox.warning(self, "Selection Error", "No disease selected.")

    def add_transmission_to_disease(self):
        selected_disease = self.disease_list.currentItem()
        new_transmission = self.new_transmission_input.text().strip().replace(" ", "_")
        selected_existing_transmission = self.transmission_combo.currentText().strip().replace(" ", "_")

        if selected_disease:
            disease_name = selected_disease.text().replace(" ", "_")
            if new_transmission:
                existing_transmissions = list(prolog.query(f"transmitted_by({disease_name}, {new_transmission})"))
                if not existing_transmissions:
                    prolog.assertz(f"transmission({new_transmission})")
                    prolog.assertz(f"transmitted_by({disease_name}, {new_transmission})")
                    self.populate_transmissions(disease_name)
                    self.new_transmission_input.clear()
                    QMessageBox.information(self, "Success", "New transmission added to disease successfully!")
                else:
                    QMessageBox.warning(self, "Warning", "Transmission already associated with the disease.")
            elif selected_existing_transmission:
                existing_transmissions = self.query_prolog(f"transmitted_by({disease_name}, {selected_existing_transmission})")
                if not existing_transmissions:
                    prolog.assertz(f"transmitted_by({disease_name}, {selected_existing_transmission})")
                    self.populate_transmissions(disease_name)
                    QMessageBox.information(self, "Success", "Transmission added to disease successfully!")
                else:
                    QMessageBox.warning(self, "Warning", "Transmission already associated with the disease.")
            else:
                QMessageBox.warning(self, "Input Error", "No transmission specified.")
        else:
            QMessageBox.warning(self, "Selection Error", "No disease selected.")


    def remove_symptom_from_disease(self):
        selected_disease = self.disease_list.currentItem()
        selected_symptom = self.symptom_combo.currentText().strip().replace(" ", "_")

        if selected_disease and selected_symptom:
            disease_name = selected_disease.text().replace(" ", "_")
            prolog.retract(f"has_symptom({disease_name}, {selected_symptom})")
            self.populate_symptoms(disease_name)
            QMessageBox.information(self, "Success", "Symptom removed from disease successfully!")
        else:
            QMessageBox.warning(self, "Selection Error", "No disease or symptom selected.")

    def remove_transmission_from_disease(self):
        selected_disease = self.disease_list.currentItem()
        selected_transmission = self.transmission_combo.currentText().strip().replace(" ", "_")

        if selected_disease and selected_transmission:
            disease_name = selected_disease.text().replace(" ", "_")
            prolog.retract(f"transmitted_by({disease_name}, {selected_transmission})")
            self.populate_transmissions(disease_name)
            QMessageBox.information(self, "Success", "Transmission removed from disease successfully!")
        else:
            QMessageBox.warning(self, "Selection Error", "No disease or transmission selected.")

    def query_prolog(self, query):
        result = list(prolog.query(query))
        return [list(entry.values())[0] for entry in result]
    


class GradientWidget(QWidget):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def paintEvent(self, event):
        painter = QPainter(self)
        gradient = QLinearGradient(0, 0, self.width(), self.height())
        gradient.setColorAt(0.0, QColor(80, 213, 183))  
        gradient.setColorAt(0.0, QColor(6, 125, 104))  
        brush = QBrush(gradient)
        painter.setBrush(brush)
        painter.drawRect(self.rect())


class CustomButton(QPushButton):
    def __init__(self, text, parent=None):
        super().__init__(text, parent)
        self.setStyleSheet("""
            QPushButton {
                font-size: 16px;
                padding: 12px;
                background-color: #00796b;
                color: white;
                border: none;
                border-radius: 5px;
            }
            QPushButton:hover {
                background-color: #00796b; /* Darker color on hover */
            }
            QPushButton:pressed {
                background-color: #003d33; /* Even darker color when pressed */
            }
        """)
        self.setCursor(QCursor(Qt.PointingHandCursor))

class MedicalDiagnosisApp(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("SympBot: Logic-based Medical Diagnosis System")
        self.setGeometry(100, 100, 900, 700)
        self.setStyleSheet("background-color: #292929;")

        
        # Initialize checkboxes dictionary
        self.checkboxes = {}

        # Create a gradient background widget
        self.background_widget = QWidget()
        
        # Create the central widget and set it as the background widget
        self.central_widget = QStackedWidget()
        self.central_widget.addWidget(self.background_widget)
        self.setCentralWidget(self.central_widget)

        # Initialize other widgets
        self.main_menu_widget = QWidget()
        self.symptoms_disease_widget = QWidget()
        self.diseases_symptom_widget = QWidget()
        self.transmission_disease_widget = QWidget()
        self.identify_disease_widget = QWidget()
        self.manage_data_widget = QWidget()

        self.init_main_menu()
        self.init_symptoms_disease()
        self.init_diseases_symptom()
        self.init_transmission_disease()
        self.init_identify_disease()
        self.init_manage_data()

        # Add widgets to the central stacked widget
        self.central_widget.addWidget(self.main_menu_widget)
        self.central_widget.addWidget(self.symptoms_disease_widget)
        self.central_widget.addWidget(self.diseases_symptom_widget)
        self.central_widget.addWidget(self.transmission_disease_widget)
        self.central_widget.addWidget(self.identify_disease_widget)
        self.central_widget.addWidget(self.manage_data_widget)
        self.central_widget.setCurrentWidget(self.main_menu_widget)




    def style_combobox(self, combobox: QComboBox):
        combobox.setStyleSheet("""
            QComboBox {
                border: 2px solid #80cbc4;  /* Light teal border for dark mode */
                border-radius: 5px;
                padding: 5px 10px;
                font-size: 14px;
                background-color: #1e1e1e;  /* Dark background */
                color: #ffffff;  /* Light text color for readability */
            }
            QComboBox::drop-down {
                border-left: 2px solid #80cbc4;  /* Match the border color */
                width: 20px;  /* Size of the dropdown arrow */
            }
            QComboBox::down-arrow {
                image: url(down_arrow_light.png);  /* Custom down arrow image suitable for dark mode */
            }
            QComboBox::drop-down:hover {
                border: 2px solid #4db6ac;  /* Slightly brighter teal on hover */
            }
            QComboBox::drop-down:focus {
                border: 2px solid #4db6ac;  /* Slightly brighter teal on focus */
            }
            QComboBox QAbstractItemView {
                border: 2px solid #80cbc4;  /* Match the border color */
                background-color: #1e1e1e;  /* Dark background for the drop-down list */
                color: #ffffff;  /* Light text color for the drop-down list */
                selection-background-color: #4db6ac;  /* Background color for selected item */
                selection-color: #1e1e1e;  /* Text color for selected item */
                padding: 5px 10px;
                margin-bottom: 5px;
            }
        """)


    def init_main_menu(self):
        # Create the gradient background widget
        gradient_background = QWidget()
        gradient_background.setLayout(QVBoxLayout())  # Use a QVBoxLayout for the gradient background

        # Create a widget for the main menu content
        content_widget = QWidget()
        content_layout = QVBoxLayout(content_widget)
        
        # Title Label
        title = QLabel("SYMPBOT")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("""
            QLabel {
                font-size: 50px;
                font-weight: bold;
                color: #00796b;

                
                font-family: 'Segoe UI', sans-serif;
                background: white;
                border-radius: 15px; /* Rounded corners */
                padding: 10px;
                border: 10px solid #00796b; /* Optional border to enhance glass effect */
            }
        """)

        content_layout.addWidget(title)
        
        # Spacer
        content_layout.addStretch(1)
        
        # Instructions Label
        instructions = QLabel("How may I help you?")
        instructions.setStyleSheet("""
            QLabel {
                font-size: 20px;
                font-weight: bold;
                color: white;
                font-family: 'Segoe UI', sans-serif;

            }
        """)

        instructions.setAlignment(Qt.AlignCenter)
        content_layout.addWidget(instructions)
        
        # Add spacer to separate labels from buttons
        content_layout.addStretch(1)
        
        # Buttons
        buttons = [
            ("Diagnose based on symptoms", self.show_identify_disease),
            ("Given a disease, find the symptoms", self.show_symptoms_disease),
            ("Given a symptom, find the possible diseases", self.show_diseases_symptom),
            ("Check Transmission Methods of a Disease", self.show_transmission_disease),
            ("Manage Data", self.show_manage_data),
            ("Exit", self.close)
        ]
        
        for text, slot in buttons:
            button = CustomButton(text)
            button.clicked.connect(slot)
            content_layout.addWidget(button)
        
        # Add spacer to keep buttons from sticking to the bottom
        content_layout.addStretch(1)

        # Add the content widget to the gradient background
        gradient_background.layout().addWidget(content_widget)

        # Set the main menu widget layout to the gradient background widget
        self.main_menu_widget.setLayout(QVBoxLayout())
        self.main_menu_widget.layout().addWidget(gradient_background)

    def populate_diseases(self, disease_combobox):
        diseases = self.query_prolog("disease(Disease)")
        disease_combobox.clear() 
        disease_combobox.addItems(diseases)

    def populate_symptoms(self, symptom_combobox):
        symptoms = self.query_prolog("symptom(Symptom)")
        symptom_combobox.clear()
        symptoms = sorted([symptom.replace("_", " ") for symptom in symptoms])
        symptom_combobox.addItems(symptoms)

    def init_symptoms_disease(self):
        layout = QVBoxLayout()
        self.symptoms_disease_widget.setLayout(layout)

        title = QLabel("Check Symptoms Associated with a Disease")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("font-size: 20px; font-weight: bold; color: #00796b;")
        layout.addWidget(title)

        self.symptoms_disease_combobox = QComboBox()
        self.populate_diseases(self.symptoms_disease_combobox)
        self.style_combobox(self.symptoms_disease_combobox)
        layout.addWidget(self.symptoms_disease_combobox)

        self.symptoms_result_area = QTextBrowser()
        self.symptoms_result_area.setStyleSheet("""
            QTextBrowser {
                background-color: transparent;
                border: none;
            }
        """)
        self.symptoms_result_area.setReadOnly(True)
        self.symptoms_result_area.setOpenExternalLinks(True)
        layout.addWidget(self.symptoms_result_area)

        buttons = [
            ("Check", self.check_symptoms),
            ("Back to Menu", self.show_main_menu)
        ]
        
        button_layout = QHBoxLayout()
        for text, slot in buttons:
            button = CustomButton(text)
            button.clicked.connect(slot)
            button_layout.addWidget(button)
        
        layout.addLayout(button_layout)

    def init_diseases_symptom(self):
        layout = QVBoxLayout()
        self.diseases_symptom_widget.setLayout(layout)

        title = QLabel("Check Diseases with a Specific Symptom")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("font-size: 20px; font-weight: bold; color: #00796b;")
        layout.addWidget(title)

        self.symptom_combobox = QComboBox()
        self.populate_symptoms(self.symptom_combobox)
        self.style_combobox(self.symptom_combobox)
        layout.addWidget(self.symptom_combobox)

        self.diseases_result_area = QTextBrowser()
        self.diseases_result_area.setReadOnly(True)
        self.diseases_result_area.setOpenExternalLinks(True)
        self.diseases_result_area.setStyleSheet("""
            QTextBrowser {
                background-color: transparent;
                border: none;
            }
        """)
        layout.addWidget(self.diseases_result_area)

        buttons = [
            ("Check", self.check_diseases),
            ("Back to Menu", self.show_main_menu)
        ]
        
        button_layout = QHBoxLayout()
        for text, slot in buttons:
            button = CustomButton(text)
            button.clicked.connect(slot)
            button_layout.addWidget(button)
        
        layout.addLayout(button_layout)

    def init_transmission_disease(self):
        layout = QVBoxLayout()
        self.transmission_disease_widget.setLayout(layout)

        title = QLabel("Check Transmission Methods of a Disease")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("font-size: 20px; font-weight: bold; color: #00796b;")
        layout.addWidget(title)

        self.disease_transmission_combobox = QComboBox()
        self.populate_diseases(self.disease_transmission_combobox)
        self.style_combobox(self.disease_transmission_combobox)
        layout.addWidget(self.disease_transmission_combobox)

        self.transmission_result_area = QTextBrowser()
        self.transmission_result_area.setReadOnly(True)
        self.transmission_result_area.setOpenExternalLinks(True)
        self.transmission_result_area.setStyleSheet("""
            QTextBrowser {
                background-color: transparent;
                border: none;
            }
        """)
        layout.addWidget(self.transmission_result_area)

        buttons = [
            ("Check", self.check_transmission),
            ("Back to Menu", self.show_main_menu)
        ]
        
        button_layout = QHBoxLayout()
        for text, slot in buttons:
            button = CustomButton(text)
            button.clicked.connect(slot)
            button_layout.addWidget(button)
        
        layout.addLayout(button_layout)


    def refresh_checkboxes(self):
        # Remove old checkboxes
        for checkbox in list(self.checkboxes.values()):
            self.symptom_checkbox_layout.removeWidget(checkbox)
            checkbox.deleteLater()
        self.checkboxes.clear()

        # Retrieve updated symptoms from Prolog
        self.all_symptoms = self.query_prolog("symptom(Symptom)")
        
        # Add new checkboxes
        for symptom in self.all_symptoms:
            checkbox = QCheckBox(symptom)
            checkbox.setStyleSheet("""
                font-size: 20px; 
                padding: 5px; 
                border-radius: 3px;
                color: white;  
                font-family: 'Segoe UI', sans-serif;      
            """)
            self.checkboxes[symptom] = checkbox
            self.symptom_checkbox_layout.addWidget(checkbox)
    
    def init_identify_disease(self):
        layout = QVBoxLayout()
        self.identify_disease_widget.setLayout(layout)

        title = QLabel("Identify Disease Based on Symptoms")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("font-size: 20px; font-weight: bold; color: white;")
        layout.addWidget(title)

        # Create a search bar
        self.search_bar = QLineEdit()
        self.search_bar.setPlaceholderText("Search symptoms...")
        self.search_bar.setStyleSheet("""
            font-size: 14px; 
            padding: 30px; 
            border: 1px solid #00796b; 
            color: white;  
            font-family: 'Segoe UI', sans-serif;                          
        """)
        self.search_bar.textChanged.connect(self.filter_symptoms)
        layout.addWidget(self.search_bar)

        # Create a widget to hold the checkboxes
        self.symptom_checkbox_widget = QWidget() 
        self.symptom_checkbox_layout = QVBoxLayout()
        self.symptom_checkbox_layout.setAlignment(Qt.AlignTop)
        self.symptom_checkbox_widget.setLayout(self.symptom_checkbox_layout)

        # Add a scroll area to handle many checkboxes
        scroll_area = QScrollArea()
        scroll_area.setMaximumHeight(200) 
        scroll_area.setWidgetResizable(True)
        scroll_area.setWidget(self.symptom_checkbox_widget)
        layout.addWidget(scroll_area)

        self.disease_identification_result_area = QTextBrowser()
        self.disease_identification_result_area.setReadOnly(True)
        self.disease_identification_result_area.setOpenExternalLinks(True)
        self.disease_identification_result_area.setStyleSheet("""
            QTextBrowser {
                background-color: transparent;
                border: none;
            }
        """)
        layout.addWidget(self.disease_identification_result_area)

        buttons = [
            ("Identify", self.identify_disease),
            ("Back to Menu", self.show_main_menu)
        ]
        
        button_layout = QHBoxLayout()
        for text, slot in buttons:
            button = CustomButton(text)
            button.clicked.connect(slot)
            button_layout.addWidget(button)
        
        layout.addLayout(button_layout)

        # Initial population of checkboxes
        self.refresh_checkboxes()


    def filter_symptoms(self):
        query = self.search_bar.text().lower()
        for symptom, checkbox in self.checkboxes.items():
            checkbox.setVisible(query in symptom.lower())

    def init_manage_data(self):
        layout = QVBoxLayout()
        self.manage_data_widget.setLayout(layout)

        title = QLabel("Manage Medical Data")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("font-size: 20px; font-weight: bold; color: #00796b;")
        layout.addWidget(title)

        # Adding buttons for different actions
        buttons = [
            ("Manage diseases", self.manage_dialog),
            
        ]

        for text, slot in buttons:
            button = CustomButton(text)
            button.clicked.connect(slot)
            layout.addWidget(button)

        # Back to Menu button
        back_button = CustomButton("Back to Menu")
        back_button.clicked.connect(self.show_main_menu)
        layout.addWidget(back_button)


    def query_prolog(self, query):
        results = list(prolog.query(query))
        return [list(entry.values())[0] for entry in results]
    

    def format_chat_bubble(self, speaker, message, is_user=False):
        """Format a message as a chat bubble with a user-friendly timestamp and optional user styling."""
        # Get current time for timestamp
        timestamp = datetime.now().strftime("%b %d, %Y %I:%M %p")  # Example: Jul 21, 2024 02:30 PM
        message = message.replace('_', " ")
        # Define colors and styles
        user_bubble_color = "#a8e6cf"  # Lighter green for user messages
        other_bubble_color = "#dcedc1"  # Light gray for other messages
        font_family = "Segoe UI, sans-serif"
        bubble_border_radius = "100%"
        bubble_padding = "12px 20px"
        bubble_margin = "8px"
        max_width = "80%"
        box_shadow = "0 4px 8px rgba(0, 0, 0, 0.2)"
        font_size = "14px"
        timestamp_color = "#555"

        bubble_style = f"""
        background-color: {other_bubble_color}; 
        border-radius: {bubble_border_radius}; 
        border: 10px solid white;
        padding: {bubble_padding}; 
        margin: {bubble_margin}; 
        max-width: {max_width};
        display: inline-block; 
        box-shadow: {box_shadow};
        text-align: left;
        word-wrap: break-word;
        font-family: {font_family}; 
        font-size: {font_size};
        """

        if is_user:
            bubble_style = f"""
            background-color: {user_bubble_color}; 
            border-radius: {bubble_border_radius}; 
            padding: {bubble_padding}; 
            margin: {bubble_margin}; 
            max-width: {max_width};
            display: inline-block; 
            box-shadow: {box_shadow};
            text-align: left;
            word-wrap: break-word;
            border: 50px solid white;

            font-family: {font_family}; 
            font-size: {font_size};
            """

        return f"""
        <div style="{bubble_style}">
            <br>
            <strong>{speaker}</strong> 
            <br>
            {message}
            <br>
            <span style="font-size: 11px; color: {timestamp_color};">{timestamp}</span>
            <br>
        </div>
        """

    
    def check_symptoms(self):
        disease = self.symptoms_disease_combobox.currentText()
        symptoms = self.query_prolog(f"has_symptom({disease}, Symptom)")
        if symptoms:
            result = self.format_chat_bubble("System", f"Symptoms associated with {disease}: {', '.join(symptoms)}")
        else:
            result = self.format_chat_bubble("System", f"No symptoms found for {disease}.")

        prompt = self.format_chat_bubble("User", f"What are the symptoms of {disease}?", is_user=True)

        existing_content = self.symptoms_result_area.toHtml()
        updated_content = existing_content + prompt + result
        self.symptoms_result_area.setHtml(updated_content)

    def check_diseases(self):
        symptom = self.symptom_combobox.currentText()
        diseases = self.query_prolog(f"disease(Disease), has_symptom(Disease, {symptom})")
        if diseases:
            result = self.format_chat_bubble("System", f"Diseases with symptom '{symptom}': {', '.join(diseases)}")
        else:
            result = self.format_chat_bubble("System", f"No diseases found with symptom '{symptom}'.")

        prompt = self.format_chat_bubble("User", f"What diseases have the symptom '{symptom}'?", is_user=True)
        
        existing_content = self.diseases_result_area.toHtml()
        updated_content = existing_content + prompt + result
        self.diseases_result_area.setHtml(updated_content)

    def check_transmission(self):
        disease = self.disease_transmission_combobox.currentText()
        transmission_methods = self.query_prolog(f"transmitted_by({disease}, Method)")
        if transmission_methods:
            result = self.format_chat_bubble("System", f"Transmission methods for {disease}: {', '.join(transmission_methods)}")
        else:
            result = self.format_chat_bubble("System", f"No transmission methods found for {disease}.")

        prompt = self.format_chat_bubble("User", f"What are the transmission methods of {disease}?", is_user=True)

        existing_content = self.transmission_result_area.toHtml()
        updated_content = existing_content + prompt + result
        self.transmission_result_area.setHtml(updated_content)


    def identify_disease(self):
        selected_symptoms = [symptom for symptom, checkbox in self.checkboxes.items() if checkbox.isChecked()]
        if not selected_symptoms:
            QMessageBox.warning(self, "Warning", "No symptoms selected.")
            return
        
        query = " , ".join([f"has_symptom(Disease, {symptom})" for symptom in selected_symptoms])
        diseases = self.query_prolog(f"disease(Disease), {query}")
        if diseases:
            result = self.format_chat_bubble("System", f"Possible diseases: {', '.join(diseases)}")
        else:
            result = self.format_chat_bubble("System", "No diseases found for the given symptoms.")
        
        prompt = self.format_chat_bubble("User", f"Identify the disease based on the following symptoms: {', '.join(selected_symptoms)}", is_user=True)

        existing_content = self.disease_identification_result_area.toHtml()
        updated_content = existing_content + prompt + result
        self.disease_identification_result_area.setHtml(updated_content)

    def manage_dialog(self):
        dialog = DiseaseManager()
        dialog.exec_()
        self.populate_symptoms(self.symptom_combobox)
        self.populate_diseases(self.symptoms_disease_combobox)
        self.populate_diseases(self.disease_transmission_combobox)
        self.refresh_checkboxes()
        
 
        
    def show_main_menu(self):
        self.central_widget.setCurrentWidget(self.main_menu_widget)

    def show_symptoms_disease(self):
        self.central_widget.setCurrentWidget(self.symptoms_disease_widget)

    def show_diseases_symptom(self):
        self.central_widget.setCurrentWidget(self.diseases_symptom_widget)

    def show_transmission_disease(self):
        self.central_widget.setCurrentWidget(self.transmission_disease_widget)

    def show_identify_disease(self):
        self.central_widget.setCurrentWidget(self.identify_disease_widget)

    def show_manage_data(self):
        self.central_widget.setCurrentWidget(self.manage_data_widget)

    def show_error_message(self, message):
        QMessageBox.critical(self, "Error", message)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MedicalDiagnosisApp()
    window.show()
    sys.exit(app.exec_())
