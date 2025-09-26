/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma once

#include <QStyledItemDelegate>
#include <QApplication>
#include <QStyleOptionButton>
#include <QAbstractItemView>
#include <QMouseEvent>
#include <QPushButton>
#include <QPainter>
#include <QPersistentModelIndex>

class EditButtonDelegate : public QStyledItemDelegate
{
    Q_OBJECT
public:
    using QStyledItemDelegate::QStyledItemDelegate;

    /* Enable / disable removal of the hover-highlight painted by Qt */
    void setSuppressHover(bool on) { m_suppressHover = on; }

private:
    mutable QPersistentModelIndex m_pressedIndex;   // index kept pressed
    bool m_suppressHover = false;                   // when true, no hover bg

public:
    /* Draw the button */
    void paint(QPainter* painter,
               const QStyleOptionViewItem& option,
               const QModelIndex& index) const override
    {
        /* ------------------------------------------------------------------
         * First let the base delegate paint the background (selection,
         * alternate-row etc.) so the colours come from the current style /
         * stylesheet.  We blank out text & icon so it will only draw the
         * background.
         * ---------------------------------------------------------------- */
        QStyleOptionViewItem bgOpt(option);
        initStyleOption(&bgOpt, index);
        if (m_suppressHover)
            bgOpt.state &= ~QStyle::State_MouseOver;    // strip hover flag
        bgOpt.text.clear();
        bgOpt.icon = QIcon();
        QStyledItemDelegate::paint(painter, bgOpt, index);

        /* ------------------------------------------------------------------
         * Use an actual QPushButton to let the current style / stylesheet do
         * all the drawing so the button appearance is consistent.
         * ---------------------------------------------------------------- */
        static QPushButton proxyBtn;
        proxyBtn.setText("...");
        proxyBtn.setEnabled(option.state & QStyle::State_Enabled);

        const QRect cellRect = option.rect.adjusted(4, 4, -4, -4);
        proxyBtn.resize(cellRect.size());

        /* Build a style option from the proxy widget and merge in state
           coming from the view (hover, pressed, focus). */
        QStyleOptionButton btnOpt;
        btnOpt.initFrom(&proxyBtn);
        btnOpt.text   = proxyBtn.text();          // ensure label is painted
        btnOpt.rect   = QRect(QPoint(0, 0), proxyBtn.size());
        btnOpt.state |= option.state & QStyle::State_HasFocus;
        // Always propagate hover state to the button itself even when
        // background hover suppression is enabled.
        btnOpt.state |= option.state & QStyle::State_MouseOver;

        /* Add pressed state if this is the index we stored on mouse-press */
        if (m_pressedIndex == index && (QApplication::mouseButtons() & Qt::LeftButton))
        {
            btnOpt.state |= QStyle::State_Sunken;
            btnOpt.state &= ~QStyle::State_Raised;
        }
        else
        {
            btnOpt.state |= QStyle::State_Raised;
            btnOpt.state &= ~QStyle::State_Sunken;
        }

        /* Render the button at the correct position inside the cell. */
        painter->save();
        painter->translate(cellRect.topLeft());
        proxyBtn.style()->drawControl(QStyle::CE_PushButton, &btnOpt, painter, &proxyBtn);
        painter->restore();
    }

    /* Handle mouse / key activation */
    bool editorEvent(QEvent* event,
                     QAbstractItemModel* model,
                     const QStyleOptionViewItem& option,
                     const QModelIndex& index) override
    {
        if (!(index.flags() & Qt::ItemIsEnabled))
            return false;

        switch (event->type())
        {
        case QEvent::MouseButtonPress:
        {
            auto* me = static_cast<QMouseEvent*>(event);
            if (option.rect.contains(me->pos()) && me->button() == Qt::LeftButton)
            {
                m_pressedIndex = index;                         // remember
                if (auto* v = qobject_cast<QAbstractItemView*>(parent()))
                    v->update(index);                           // repaint pressed
                return true;                                    // handled
            }
            break;
        }
        case QEvent::MouseButtonRelease:
        {
            auto* me = static_cast<QMouseEvent*>(event);
            if (option.rect.contains(me->pos()) && me->button() == Qt::LeftButton)
            {
                bool res = model->setData(index, QVariant{}, Qt::EditRole);
                m_pressedIndex = QPersistentModelIndex();       // clear
                if (auto* v = qobject_cast<QAbstractItemView*>(parent()))
                    v->update(index);
                return res;
            }
            break;
        }
        case QEvent::KeyPress:
        {
            auto* ke = static_cast<QKeyEvent*>(event);
            if (ke->key() == Qt::Key_Space || ke->key() == Qt::Key_Return)
            {
                return model->setData(index, QVariant{}, Qt::EditRole);
            }
            break;
        }
        default:
            break;
        }
        return false;
    }
};
