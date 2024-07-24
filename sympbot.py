from datetime import datetime
from PyQt5.QtWidgets import QApplication, QMainWindow, QPushButton, QLabel, QWidget, QComboBox, QScrollArea, QCheckBox, QVBoxLayout, QTableWidget, QTableWidgetItem, QComboBox, QTextBrowser, QLineEdit, QHBoxLayout, QMessageBox
from PyQt5.QtGui import QFont, QFontDatabase, QCursor
from PyQt5.QtCore import Qt
from pyswip import Prolog

class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        # Load the Poppins font
        font_path = "C:/Users/USER/Desktop/Poppins-Regular.ttf"  # Update this path if necessary
        font_id = QFontDatabase.addApplicationFont(font_path)
        font_families = QFontDatabase.applicationFontFamilies(font_id)
        font_family = font_families[0] if font_families else "Arial"  # Fallback font

        # Set up the main window with fixed size
        self.setWindowTitle("SympBot Medical Diagnosis System")
        self.setFixedSize(800, 600)  # Fixed size for the window

        self.central_widget = QWidget()
        self.setCentralWidget(self.central_widget)

        # Hi! I’m Sympbot
        self.title_label = QLabel("Hi! I’m Sympbot", self)
        self.title_font = QFont(font_family, 35)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignCenter)
        self.title_label.setGeometry(100, 200, 600, 100)  # x, y, width, height

        # Medical Diagnosis Expert System
        self.sub_title_label = QLabel("Medical Diagnosis Expert System", self)
        self.sub_title_font = QFont(font_family, 10)
        self.sub_title_label.setFont(self.sub_title_font)
        self.sub_title_label.setAlignment(Qt.AlignCenter)
        self.sub_title_label.setStyleSheet("color: #B7B7B7")
        self.sub_title_label.setGeometry(100, 290, 600, 30)  # x, y, width, height

        # Diagnose Button
        self.diagnose_button = QPushButton("Diagnose", self)
        self.diagnose_button_font = QFont(font_family, 9)
        self.diagnose_button.setFont(self.diagnose_button_font)
        self.diagnose_button.setStyleSheet("background-color: #91C4B3; border-radius: 15px; padding: 10%;")
        self.diagnose_button.setGeometry(275, 370, 100, 40)  # x, y, width, height
        self.diagnose_button.clicked.connect(self.open_diagnose_page)
        self.diagnose_button.setCursor(QCursor(Qt.PointingHandCursor))

        # Data Button
        self.data_button = QPushButton("Data", self)
        self.data_button_font = QFont(font_family, 9)
        self.data_button.setFont(self.data_button_font)
        self.data_button.setStyleSheet("background-color: #B3DEF5; border-radius: 15px; padding: 10px;")
        self.data_button.setGeometry(425, 370, 100, 40)  # x, y, width, height
        self.data_button.clicked.connect(self.open_data_page)
        self.data_button.setCursor(QCursor(Qt.PointingHandCursor))

        self.setStyleSheet("background-color: white;")
        
    def open_diagnose_page(self):
        self.diagnose_page = DiagnosePage()
        self.diagnose_page.show()
        self.close()

    def open_data_page(self):
        # Functionality for Data button
        self.data_page = DataPage()
        self.data_page.show()
        self.close()

class DataPage(QWidget):
    def __init__(self):
        super().__init__()

        self.font_path = "C:/Users/USER/Desktop/Poppins-Regular.ttf"  # Update this path if necessary
        self.font_id = QFontDatabase.addApplicationFont(self.font_path)
        self.font_families = QFontDatabase.applicationFontFamilies(self.font_id)
        self.font_family = self.font_families[0]

        # Set up the data page
        self.setWindowTitle("Data Management Page")
        self.setFixedSize(800, 600)  # Fixed size for the window

        # Table to display diseases
        self.disease_table = QTableWidget(self)
        self.disease_table.setColumnCount(1)  # One column for Disease Name
        self.disease_table.setHorizontalHeaderLabels(["Disease Name"])
        self.disease_table.setGeometry(50, 50, 700, 400)  # x, y, width, height
        self.disease_table.setSelectionBehavior(QTableWidget.SelectRows)

        # Input field for disease name
        self.disease_name_input = QLineEdit(self)
        self.disease_name_input.setPlaceholderText("Disease Name")
        self.disease_name_input.setGeometry(50, 470, 300, 30)  # x, y, width, height

        # Buttons for CRUD operations
        self.create_button = QPushButton("Create", self)
        self.create_button.setGeometry(370, 470, 80, 30)  # x, y, width, height
        self.create_button.clicked.connect(self.create_disease)

        self.update_button = QPushButton("Update", self)
        self.update_button.setGeometry(460, 470, 80, 30)  # x, y, width, height
        self.update_button.clicked.connect(self.update_disease)

        self.delete_button = QPushButton("Delete", self)
        self.delete_button.setGeometry(550, 470, 80, 30)  # x, y, width, height
        self.delete_button.clicked.connect(self.delete_disease)

        # Back button
        self.back_button = QPushButton("Back", self)
        self.back_button.setGeometry(50, 525, 80, 30)  # x, y, width, height
        self.back_button.clicked.connect(self.go_back)

        # Symptoms button
        self.symptoms_button = QPushButton("Symptoms", self)
        self.symptoms_button.setGeometry(150, 525, 100, 30)  # x, y, width, height
        self.symptoms_button.clicked.connect(self.open_symptoms_page)

        self.setStyleSheet("background-color: white;")
        self.populate_table()

    def populate_table(self):
        # Example: replace with actual Prolog query to get diseases
        diseases = pl.query_prolog("disease(Name)")

        self.disease_table.setRowCount(len(diseases))
        for row, name in enumerate(diseases):
            self.disease_table.setItem(row, 0, QTableWidgetItem(name))

    def create_disease(self):
        name = self.disease_name_input.text()
        if name:
            # Add Prolog query to create disease
            pl.prolog.assertz(f"disease('{name}')")
            self.populate_table()
        else:
            # Optionally show a warning message
            print("Please enter a Disease Name.")

    def update_disease(self):
        selected_row = self.disease_table.currentRow()
        if selected_row >= 0:
            old_name = self.disease_table.item(selected_row, 0).text()
            new_name = self.disease_name_input.text()

            if new_name:
                old_has_symptom_query = f"has_symptom({old_name}, Symptom)"
                for symptom in pl.query_prolog(old_has_symptom_query):
                    
                    pl.prolog.retract(f"has_symptom({old_name}, {symptom})")
                    pl.prolog.assertz(f"has_symptom({new_name}, {symptom})")

             
                old_symptom_weight_query = f"symptom_weight('{old_name}', Symptom, Weight)"
                for result in pl.raw_query(old_symptom_weight_query):
                    symptom = result['Symptom']
                    weight = result['Weight']

                    pl.prolog.retract(f"symptom_weight({old_name}, {symptom}, {weight})")
                    pl.prolog.assertz(f"symptom_weight({new_name}, {symptom}, {weight})")


                pl.prolog.retract(f"disease({old_name})")
                pl.prolog.assertz(f"disease({new_name})")


                self.populate_table()
            else:
                # Optionally show a warning message
                print("Please enter a new Disease Name.")
        else:
            # Optionally show a warning message
            print("Please select a disease to update.")


    def delete_disease(self):
        selected_row = self.disease_table.currentRow()
        if selected_row >= 0:
            name = self.disease_table.item(selected_row, 0).text()
            # Delete Prolog entry
            pl.prolog.retract(f"disease('{name}')")
            self.populate_table()
        else:
            # Optionally show a warning message
            print("Please select a disease to delete.")

    def go_back(self):
        self.main_window = MainWindow()
        self.main_window.show()
        self.close()

    def open_symptoms_page(self):
        self.symptomsCRUD = SymptomCRUDPage()
        self.symptomsCRUD.show()
        self.close()

class SymptomCRUDPage(QWidget):
    def __init__(self, parent=None):
        super(SymptomCRUDPage, self).__init__(parent)
        
        # Layout
        layout = QVBoxLayout()

        # Symptom Management
        self.symptom_table = QTableWidget(0, 1)
        self.symptom_table.setHorizontalHeaderLabels(['Symptom'])
        layout.addWidget(self.symptom_table)

        self.symptom_input = QLineEdit()
        layout.addWidget(QLabel('Symptom Name:'))
        layout.addWidget(self.symptom_input)

        button_layout = QHBoxLayout()
        self.add_button = QPushButton('Add Symptom')
        self.update_button = QPushButton('Update Symptom')
        self.delete_button = QPushButton('Delete Symptom')
        self.back_button = QPushButton('Back to Menu')

        button_layout.addWidget(self.add_button)
        button_layout.addWidget(self.update_button)
        button_layout.addWidget(self.delete_button)
        button_layout.addWidget(self.back_button)

        layout.addLayout(button_layout)

        self.add_button.clicked.connect(self.add_symptom)
        self.update_button.clicked.connect(self.update_symptom)
        self.delete_button.clicked.connect(self.delete_symptom)
        self.back_button.clicked.connect(self.go_back)

        # Load existing symptoms
        self.populate_symptom_table()

        # Disease-Symptom Association Management
        self.disease_dropdown = QComboBox()
        diseases = list(pl.prolog.query("disease(X)"))
        for disease in diseases:
            self.disease_dropdown.addItem(disease['X'])
        layout.addWidget(QLabel('Select Disease:'))
        layout.addWidget(self.disease_dropdown)

        self.association_table = QTableWidget(0, 2)
        self.association_table.setHorizontalHeaderLabels(['Associated Symptom', 'Weight'])
        layout.addWidget(self.association_table)

        self.association_input = QLineEdit()
        layout.addWidget(QLabel('Symptom to Associate:'))
        layout.addWidget(self.association_input)

        self.weight_input = QLineEdit()
        layout.addWidget(QLabel('Weight:'))
        layout.addWidget(self.weight_input)

        association_button_layout = QHBoxLayout()
        self.add_association_button = QPushButton('Add Association')
        self.remove_association_button = QPushButton('Remove Association')

        association_button_layout.addWidget(self.add_association_button)
        association_button_layout.addWidget(self.remove_association_button)

        layout.addLayout(association_button_layout)

        self.add_association_button.clicked.connect(self.add_association)
        self.remove_association_button.clicked.connect(self.remove_association)

        self.disease_dropdown.currentIndexChanged.connect(self.populate_association_table)
        self.populate_association_table()

        self.setLayout(layout)

    def populate_symptom_table(self):
        self.symptom_table.setRowCount(0)
        symptoms = list(pl.prolog.query("symptom(X)"))
        for symptom in symptoms:
            row_position = self.symptom_table.rowCount()
            self.symptom_table.insertRow(row_position)
            self.symptom_table.setItem(row_position, 0, QTableWidgetItem(symptom['X']))

    def add_symptom(self):
        symptom_name = self.symptom_input.text()
        if symptom_name:
            pl.prolog.assertz(f"symptom('{symptom_name}')")
            self.populate_symptom_table()
            self.symptom_input.clear()
        else:
            QMessageBox.warning(self, 'Input Error', 'Please enter a symptom name.')

    def update_symptom(self):
        selected_row = self.symptom_table.currentRow()
        if selected_row >= 0:
            old_name = self.symptom_table.item(selected_row, 0).text()
            new_name = self.symptom_input.text()
            if new_name:
                # Retract old symptom and assert the new symptom
                pl.prolog.retract(f"symptom('{old_name}')")
                pl.prolog.assertz(f"symptom('{new_name}')")
                
                # Update has_symptom associations
                associations = list(pl.prolog.query(f"has_symptom(Disease, '{old_name}')"))
                for association in associations:
                    disease_name = association['Disease']
                    pl.prolog.retract(f"has_symptom('{disease_name}', '{old_name}')")
                    pl.prolog.assertz(f"has_symptom('{disease_name}', '{new_name}')")
                    
                # Update symptom_weight associations
                weights = list(pl.prolog.query(f"symptom_weight(Disease, '{old_name}', Weight)"))
                for weight in weights:
                    disease_name = weight['Disease']
                    symptom_weight = weight['Weight']
                    pl.prolog.retract(f"symptom_weight('{disease_name}', '{old_name}', {symptom_weight})")
                    pl.prolog.assertz(f"symptom_weight('{disease_name}', '{new_name}', {symptom_weight})")
                
                # Refresh the symptom table and clear the input field
                self.populate_symptom_table()
                self.symptom_input.clear()
            else:
                QMessageBox.warning(self, 'Input Error', 'Please enter a new symptom name.')
        else:
            QMessageBox.warning(self, 'Selection Error', 'Please select a symptom to update.')

    def delete_symptom(self):
        selected_row = self.symptom_table.currentRow()
        if selected_row >= 0:
            symptom_name = self.symptom_table.item(selected_row, 0).text()
            pl.prolog.retract(f"symptom('{symptom_name}')")
            self.populate_symptom_table()
        else:
            QMessageBox.warning(self, 'Selection Error', 'Please select a symptom to delete.')

    def populate_association_table(self):
        disease_name = self.disease_dropdown.currentText()
        self.association_table.setRowCount(0)
        associations = list(pl.prolog.query(f"has_symptom('{disease_name}', Symptom), symptom_weight('{disease_name}', Symptom, Weight)"))
        for association in associations:
            row_position = self.association_table.rowCount()
            self.association_table.insertRow(row_position)
            self.association_table.setItem(row_position, 0, QTableWidgetItem(association['Symptom']))
            self.association_table.setItem(row_position, 1, QTableWidgetItem(str(association['Weight'])))

    def add_association(self):
        disease_name = self.disease_dropdown.currentText()
        symptom_name = self.association_input.text()
        weight = self.weight_input.text()
        if symptom_name and weight:
            # Check if the symptom exists in the database
            symptom_exists = list(pl.prolog.query(f"symptom('{symptom_name}')"))
            if symptom_exists:
                association_exists = list(pl.prolog.query(f"has_symptom('{disease_name}', '{symptom_name}')"))

                if association_exists:
                    pl.prolog.retract(f"symptom_weight('{disease_name}', '{symptom_name}', _)")
                    pl.prolog.assertz(f"symptom_weight('{disease_name}', '{symptom_name}', {weight})")
                else:
                    pl.prolog.assertz(f"has_symptom('{disease_name}', '{symptom_name}')")
                    pl.prolog.assertz(f"symptom_weight('{disease_name}', '{symptom_name}', {weight})")
                    
                self.populate_association_table()
                self.association_input.clear()
                self.weight_input.clear()
            else:
                # If symptom does not exist, show warning
                QMessageBox.warning(self, 'Input Error', 'Symptom does not exist. Please add the symptom first.')
        else:
            QMessageBox.warning(self, 'Input Error', 'Please enter a symptom name and weight to associate.')

    def remove_association(self):
        selected_row = self.association_table.currentRow()
        if selected_row >= 0:
            disease_name = self.disease_dropdown.currentText()
            symptom_name = self.association_table.item(selected_row, 0).text()
            pl.prolog.retract(f"has_symptom('{disease_name}', '{symptom_name}')")
            pl.prolog.retract(f"symptom_weight('{disease_name}', '{symptom_name}', _)")
            self.populate_association_table()
        else:
            QMessageBox.warning(self, 'Selection Error', 'Please select an association to remove.')

    def go_back(self):
        self.main_window = MainWindow()
        self.main_window.show()
        self.close()






class DiagnosePage(QWidget):
    def __init__(self):
        super().__init__()

        font_path = "C:/Users/USER/Desktop/Poppins-Regular.ttf"  # Update this path if necessary
        font_id = QFontDatabase.addApplicationFont(font_path)
        font_families = QFontDatabase.applicationFontFamilies(font_id)
        font_family = font_families[0]

        # Set up the diagnose page
        self.setWindowTitle("Diagnose Page")
        self.setFixedSize(800, 600)  # Fixed size for the window

        # Message
        self.message_label = QLabel("It looks like you're not feeling your best right now. I’m here to help you figure out what might be going on. First off, who’s the patient?", self)
        self.message_font = QFont(font_family, 20)
        self.message_label.setFont(self.message_font)
        self.message_label.setWordWrap(True)  # Enable text wrapping
        self.message_label.setGeometry(50, 100, 700, 300)  # x, y, width, height

        # Patient dropdown
        self.patient_label = QLabel("Risk Profile:", self)
        self.patient_font = QFont(font_family, 14)
        self.patient_label.setFont(self.patient_font)
        self.patient_label.setGeometry(50, 400, 150, 30)  # x, y, width, height

        self.patient_dropdown = QComboBox(self)
        self.patient_dropdown.addItems(["Elderly", "Children", "Immunocompromised Children", "Immunocompromised Elderly", "Not a Risk Profile"])
        self.patient_dropdown.setGeometry(200, 400, 300, 30)  # x, y, width, height
        self.patient_dropdown.setFont(QFont(font_family, 10))
        self.patient_dropdown.setStyleSheet("""
            QComboBox {
                border-radius: 5px;
                padding: 5px 10px;
                background-color: transparent;
            }
        """)

        # Next button
        self.next_button = QPushButton("Next", self)
        self.next_button_font = QFont(font_family, 10)
        self.next_button.setFont(self.next_button_font)
        self.next_button.setStyleSheet("background-color: #91C4B3; border-radius: 15px; padding: 10px;")
        self.next_button.setGeometry(615, 525, 100, 40)  # x, y, width, height
        self.next_button.clicked.connect(self.open_symptom_page)

    def open_symptom_page(self):
        self.symptom_page = SymptomPage(self.patient_dropdown.currentText())
        self.symptom_page.show()
        self.close()

class SymptomPage(QWidget):
    def __init__(self, risk_profile):
        super().__init__()
        self.risk_profile = risk_profile

        self.font_path = "C:/Users/USER/Desktop/Poppins-Regular.ttf"  # Update this path if necessary
        self.font_id = QFontDatabase.addApplicationFont(self.font_path)
        self.font_families = QFontDatabase.applicationFontFamilies(self.font_id)
        self.font_family = self.font_families[0]

        # Set up the symptom page
        self.setWindowTitle("Symptom Page")
        self.setFixedSize(800, 600)  # Fixed size for the window

        # Message
        self.message_label = QLabel("To find out what might be causing the discomfort, please share your symptoms", self)
        self.message_font = QFont(self.font_family, 14)
        self.message_label.setFont(self.message_font)
        self.message_label.setWordWrap(True)  # Enable text wrapping
        self.message_label.setGeometry(50, 20, 700, 100)  # x, y, width, height

        # Symptom checkboxes
        self.symptom_layout = QVBoxLayout()
        self.scroll_area = QScrollArea(self)
        self.scroll_widget = QWidget()
        self.scroll_widget.setLayout(self.symptom_layout)
        self.scroll_area.setWidget(self.scroll_widget)
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setGeometry(50, 125, 700, 300)  # x, y, width, height
        self.populate_symptoms()

        # Next button
        self.next_button = QPushButton("Next", self)
        self.next_button_font = QFont(self.font_family, 10)
        self.next_button.setFont(self.next_button_font)
        self.next_button.setStyleSheet("background-color: #91C4B3; border-radius: 15px; padding: 10px;")
        self.next_button.setGeometry(615, 525, 100, 40)  # x, y, width, height
        self.next_button.clicked.connect(self.open_investigation_page)

    def populate_symptoms(self):
        # Example symptoms (to be replaced with Prolog query)
        symptoms = pl.populate_symptoms()

        for symptom in symptoms:
            checkbox = QCheckBox(symptom, self)
            checkbox.setStyleSheet("""
                padding: 5px;                 
                font-family: 'Poppins';
            """)
            self.checkbox_font = QFont(self.font_family, 12)
            self.symptom_layout.addWidget(checkbox)

    def open_investigation_page(self):
        symptoms = [cb.text() for cb in self.findChildren(QCheckBox) if cb.isChecked()]
        symptoms = [s.replace(" ", "_") for s in symptoms]
        symptoms_comma = ",".join(symptoms)
        symptoms = f"[{symptoms_comma}]"
        diagnosis = pl.get_diagnosis(f"diagnose({symptoms}, Diagnoses, Justifications).")
        
        self.diagnosis_page = DiagnosisPage(symptoms_comma, diagnosis)
        self.diagnosis_page.show()
        self.close()



class DiagnosisPage(QWidget):
    def __init__(self, symptoms, diagnosis):
        super().__init__()

        self.font_path = "C:/Users/USER/Desktop/Poppins-Regular.ttf"  # Update this path if necessary
        self.font_id = QFontDatabase.addApplicationFont(self.font_path)
        self.font_families = QFontDatabase.applicationFontFamilies(self.font_id)
        self.font_family = self.font_families[0]

        self.setWindowTitle("Diagnosis Page")
        self.setFixedSize(800, 600)  # Fixed size for the window

        layout = QVBoxLayout(self)
        self.setLayout(layout)

        # Results area
        self.results_area = QTextBrowser()
        self.results_area.setReadOnly(True)
        self.results_area.setOpenExternalLinks(True)
        self.results_area.setStyleSheet("""
            QTextBrowser {
                background-color: #f4f4f4;
                border: none;
                font-family: Poppins, sans-serif;
            }
        """)
        self.scroll_area = QScrollArea()
        self.scroll_area.setWidget(self.results_area)
        self.scroll_area.setWidgetResizable(True)

        layout.addWidget(self.scroll_area)

        diagnosis = [dia.split("|") for dia in diagnosis]

        self.results_area.setHtml(self.format_prescription(symptoms, diagnosis))

        # Next button
        self.next_button = QPushButton("Finish", self)
        self.next_button_font = QFont(self.font_family, 10)
        self.next_button.setFont(self.next_button_font)
        self.next_button.setStyleSheet("background-color: #91C4B3; border-radius: 15px; padding: 10px; color: #fff;")
        self.next_button.setFixedSize(100, 40)
        layout.addWidget(self.next_button)

        # Adjust spacing if necessary
        layout.setContentsMargins(20, 20, 20, 20)
        layout.setSpacing(10)

        # Optionally, you can connect the button click to a function
        self.next_button.clicked.connect(self.finish_diagnosis)
    
    def finish_diagnosis(self):
        self.main_window = MainWindow()
        self.main_window.show()
        self.close()


    def format_prescription(self, symptoms, diagnosis):
        timestamp = datetime.now().strftime("%b %d, %Y %I:%M %p")  # Example: Jul 21, 2024 02:30 PM
        # message = message.replace('_', " ")
        
        # Define colors and styles
        font_family = "Poppins, sans-serif"
        font_size = "14px"
        border_color = "#ddd"
        background_color = "transparent"
        timestamp_color = "#888"
        header_color = "#333"
        
        document_style = f"""
            width: 100%; 
            max-width: 600px; 
            padding: 20px;
            margin: 20px auto;
            border: 1px solid {border_color}; 
            border-radius: 8px;
            background-color: {background_color};
            font-family: {font_family}; 
            font-size: {font_size};
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        """
        
        header_style = f"""
            font-size: 18px; 
            font-weight: bold; 
            color: {header_color};
            margin-bottom: 10px;
        """
        
        body = f"""
        <div style="{document_style}">
            <div style="{header_style}">
                <strong>Health Report</strong>
            </div>
            <div>
                    We understand that you're currently experiencing some health issues. After evaluating the symptoms you've provided, here are the possible diagnoses that align with your condition.
                    <br>
                    <br>
                    <strong>Symptoms</strong>: <br>
                    {symptoms}
                    <br>
                    <br>
                    <strong>Findings </strong> 
            </div>
           
            """
    
        for dia in diagnosis:
            disease = dia[0].replace("_", " ")
            matching_symptoms = dia[1].replace("_", " ")
            certainty = dia[2]

            body += f"""
                <div style="margin-top: 10px;">
                    <strong>Potential Diagnosis</strong>: {disease}<br>
                    <strong>Matching Symptoms</strong>: {matching_symptoms}<br>
                    <strong>Certainty Score</strong>: {certainty}<br>
                </div>
            """

        body += f"""<div style="margin-top: 20px; font-size: 12px; color: {timestamp_color};">

                    <div>
                        Disclaimer: This diagnosis is based on the symptoms you’ve reported and is intended for informational purposes only. For an accurate diagnosis and appropriate treatment, please consult a healthcare professional.
                    </div>
                <span>{timestamp}</span>
            </div>
        </div>
        """
        return body


class PrologInterface:


    def __init__(self) -> None:
        self.prolog = Prolog()
        self.prolog.consult("med.pl")

    def populate_symptoms(self):
        symptoms = list(self.query_prolog("symptom(Symptom)"))
        return [symptom.replace("_", " ") for symptom in symptoms]
    
    def query_prolog(self, query):
        results = list(self.prolog.query(query))
        return [list(entry.values())[0] for entry in results]

    def raw_query(self, query):
        return list(self.prolog.query(query))
    

    def get_diagnosis(self, query):
        diagnosis = []
        result = list(self.prolog.query(query))
        
        for solution in result:
            print(solution)
            for justifications in solution['Justifications']:
                if(type(justifications) == str):
                    diagnosis.append(justifications)
        return diagnosis
                    

        # print(self.query_prolog(query))


if __name__ == "__main__":
    pl = PrologInterface()
    app = QApplication([])
    window = MainWindow()
    window.show()
    app.exec_()
