import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess

class TextEditor:
    def __init__(self, root):
        self.root = root
        self.root.title("Text Editor")
        self.root.geometry("800x600")

        # Create a text widget for file contents
        self.text_area = tk.Text(self.root)
        self.text_area.pack(fill="both", expand=True)

        # Create a menu bar
        self.menu_bar = tk.Menu(self.root)
        self.root.config(menu=self.menu_bar)

        # Add "Archivo" menu
        archivo_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Archivo", menu=archivo_menu)

        # Add menu items under "Archivo"
        archivo_menu.add_command(label="Nuevo", command=self.new_file)
        archivo_menu.add_command(label="Abrir", command=self.open_file)
        archivo_menu.add_command(label="Guardar", command=self.save_file)
        archivo_menu.add_command(label="Guardar como", command=self.save_file_as)
        archivo_menu.add_separator()
        archivo_menu.add_command(label="Salir", command=self.exit_editor)

        # Add "Analizar" menu to call Fortran main.exe
        analizar_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Analizar", menu=analizar_menu)
        analizar_menu.add_command(label="Enviar a Fortran", command=self.run_fortran)

        # Variable to store the current file path
        self.file_path = None

    def new_file(self):
        if self.text_area.get("1.0", tk.END).strip():
            if messagebox.askyesno("Confirm", "¿Desea guardar antes de crear un nuevo archivo?"):
                self.save_file()
        self.text_area.delete("1.0", tk.END)
        self.file_path = None

    def open_file(self):
        self.file_path = filedialog.askopenfilename(
            filetypes=[("Text Files", "*.LFP"), ("All Files", "*.*")]
        )
        if self.file_path:
            with open(self.file_path, "r") as file:
                content = file.read()
                self.text_area.delete("1.0", tk.END)
                self.text_area.insert(tk.END, content)

    def save_file(self):
        if self.file_path:
            with open(self.file_path, "w") as file:
                content = self.text_area.get("1.0", tk.END)
                file.write(content)
        else:
            self.save_file_as()

    def save_file_as(self):
        self.file_path = filedialog.asksaveasfilename(
            defaultextension=".LFP",
            filetypes=[("Text Files", "*.LFP"), ("All Files", "*.*")]
        )
        if self.file_path:
            with open(self.file_path, "w") as file:
                content = self.text_area.get("1.0", tk.END)
                file.write(content)

    def exit_editor(self):
        if messagebox.askokcancel("Salir", "¿Está seguro de que quiere salir?"):
            self.root.quit()

    def run_fortran(self):
        # Guarda el archivo antes de enviarlo a Fortran
        if self.file_path is None:
            self.save_file_as()

        if self.file_path:
            try:
                # Ejecuta main.exe y envía el archivo como argumento
                result = subprocess.run(
                    ["./Main.exe", self.file_path],  # Ruta a tu ejecutable Fortran
                    capture_output=True, text=True, check=True
                )
                # Muestra el resultado devuelto por Fortran en un mensaje
                messagebox.showinfo("Resultado del análisis", result.stdout)
            except subprocess.CalledProcessError as e:
                # Si hay un error, se muestra el error
                messagebox.showerror("Error", f"Error al ejecutar main.exe: {e.stderr}")

if __name__ == "__main__":
    root = tk.Tk()
    app = TextEditor(root)
    root.mainloop()
