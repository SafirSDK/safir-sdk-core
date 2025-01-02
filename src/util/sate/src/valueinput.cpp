/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include "valueinput.h"
#include "ui_textvalueinput.h"
#include "ui_comboboxvalueinput.h"

#include <QKeyEvent>
#include <QFocusEvent>

// ---------- TEXT --------------
TextValueInput::TextValueInput(bool showNullButton, const QString& deleteButtonText, QWidget* parent)
    : ValueInput(parent)
    , ui(new Ui::TextValueInput)
{
    ui->setupUi(this);

    if (showNullButton)
    {
        connect(ui->nullButton, &QPushButton::clicked, this, [this]
        {
            SetNull();
            emit Commit(0);
        });
    }
    else
    {
        ui->nullButton->hide();
    }

    if (!deleteButtonText.isEmpty())
    {
        connect(ui->deleteButton, &QPushButton::clicked, this, [this]
        {
            m_delete = true;
            emit Commit(0);
        });
    }
    else
    {
        ui->deleteButton->hide();
    }
    

    connect(ui->valueLineEdit, &QLineEdit::textEdited, this, [this](const QString&)
    {
        m_isNull = false;
        if (!ui->valueLineEdit->hasAcceptableInput())
        {
            ui->valueLineEdit->setStyleSheet("background:red;");
        }
        else
        {
            ui->valueLineEdit->setStyleSheet("");
        }
    });

    ui->valueLineEdit->installEventFilter(this);

    setFocusPolicy(Qt::StrongFocus);
    setFocusProxy(ui->valueLineEdit);

    ui->valueLineEdit->setFocus();
}

TextValueInput::~TextValueInput()
{
    delete ui;
}

void TextValueInput::SetNull()
{
    ui->valueLineEdit->clear();
    ui->valueLineEdit->setStyleSheet("");
    ui->valueLineEdit->setPlaceholderText("NULL");
    m_isNull = true;
}

bool TextValueInput::IsNull() const
{
    return m_isNull;
}

bool TextValueInput::IsDelete() const
{
    return m_delete;
}

void TextValueInput::SetValue(const QString& value)
{
    ui->valueLineEdit->setText(value);
    m_isNull = false;
}

QString TextValueInput::GetValue() const
{
    return ui->valueLineEdit->text();
}

void TextValueInput::SetValidator(const QValidator *validator)
{
    if (validator != nullptr)
    {
        ui->valueLineEdit->setValidator(validator);
    }
}

bool TextValueInput::HasValidInput() const
{
    return m_isNull || ui->valueLineEdit->hasAcceptableInput();
}

void TextValueInput::SetMaxLength(int len)
{
    ui->valueLineEdit->setMaxLength(len);
}

//---------------- COMBO -----------------
ComboBoxValueInput::ComboBoxValueInput(const std::vector<QString>& values, bool showNullItem, const QString& deleteButtonText, QWidget *parent)
    : ValueInput(parent)
    , ui(new Ui::ComboBoxValueInput)
    , m_showNullItem(showNullItem)
{
    ui->setupUi(this);

    if (m_showNullItem)
    {
        ui->valueComboBox->addItem("Null");
    }
    for (const auto& val : values)
    {
        ui->valueComboBox->addItem(val);
    }

    if (!deleteButtonText.isEmpty())
    {
        connect(ui->deleteButton, &QPushButton::clicked, this, [this]
                {
                    m_delete = true;
                    emit Commit(0);
                });
    }
    else
    {
        ui->deleteButton->hide();
    }

    ui->valueComboBox->installEventFilter(this);

    setFocusPolicy(Qt::StrongFocus);
    setFocusProxy(ui->valueComboBox);
    ui->valueComboBox->setFocus();
}

ComboBoxValueInput::~ComboBoxValueInput()
{
    delete ui;
}

void ComboBoxValueInput::SetNull()
{
    if (m_showNullItem)
    {
        ui->valueComboBox->setCurrentIndex(0);
    }
}

bool ComboBoxValueInput::IsNull() const
{
    return m_showNullItem && ui->valueComboBox->currentIndex() == 0;
}

void ComboBoxValueInput::SetValue(const QString& value)
{
    ui->valueComboBox->setCurrentText(value);
}

bool ComboBoxValueInput::IsDelete() const
{
    return m_delete;
}

QString ComboBoxValueInput::GetValue() const
{
    return ui->valueComboBox->currentText();
}

// ------------- VALUEinput ------------
bool ValueInput::eventFilter(QObject*, QEvent *event)
{
    if(event->type() == QEvent::KeyPress)
    {
        QKeyEvent *keyEvent = static_cast<QKeyEvent*>(event);
        if (keyEvent->key() == Qt::Key_Backtab || (keyEvent->key() == Qt::Key_Tab && keyEvent->modifiers() == Qt::ShiftModifier))
        {
            emit Commit(-1);
            return true;
        }
        else if (keyEvent->key() == Qt::Key_Tab)
        {
            emit Commit(1);
            return true;
        }
        else if (keyEvent->key() == Qt::Key_Return || keyEvent->key() == Qt::Key_Enter)
        {
            emit Commit(0);
            return true;
        }
    }

    return false;
}
